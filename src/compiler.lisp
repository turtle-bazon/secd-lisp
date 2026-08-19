;;;; compiler.lisp — Compiler for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Compiles secd-lisp AST to SECD bytecode.

(in-package #:secd-lisp)

;;; Compilation context
(defstruct (compilation-context (:constructor make-compilation-context))
  "Context for compilation."
  (constants (make-hash-table :test 'equal) :type hash-table)
  (symbols (make-hash-table :test 'eq) :type hash-table)
  (code (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0) :type vector)
  (constants-list nil :type list)
  (labels nil :type list)
  (current-label 0 :type fixnum)
  (lambdas nil :type list)  ; For nested lambda compilation
  (functions (make-hash-table :test 'eq) :type hash-table)  ; name -> function body address
  (primitives (make-hash-table :test 'eq) :type hash-table)  ; name -> primitive id
  (prim-arities (make-hash-table :test 'eq) :type hash-table)  ; name -> declared arg count (def-c-fun)
  (scopes nil :type list)  ; lexical scopes: list of frames; each frame is a list of bound names (innermost frame first)
  (current-package "SECD" :type string)  ; package symbols resolve in
  (packages (make-hash-table :test 'equal) :type hash-table)  ; package name -> (required refers exports)
  (top-level-constants (make-hash-table :test 'equal) :type hash-table)  ; canon name -> literal value
  (top-level-variables (make-hash-table :test 'equal) :type hash-table)  ; canon name -> global cell index
  (top-level-variable-order nil :type list)  ; canonical names in declaration order (global frame layout)
  (top-level-variable-inits (make-hash-table :test 'equal) :type hash-table)  ; canon name -> init AST node (or nil)
  (bytevec-pool nil :type list)  ; byte-vector literals, each a list of byte values; slot order = list order (deduped, equal test)
  (bytevec-slots (make-hash-table :test 'equal) :type hash-table)  ; pool bytes -> slot index
  (entry "SECD:MAIN" :type string))  ; entry point function, "PKG:FN", chosen at build time

;;; Bytecode instructions (matching secd-machine bytecode.h)
(defconstant +op-stop+ #x00)
(defconstant +op-ldm+ #x01)
(defconstant +op-ldc+ #x02)
(defconstant +op-ldf+ #x03)
(defconstant +op-lde+ #x04)
(defconstant +op-add+ #x10)
(defconstant +op-sub+ #x11)
(defconstant +op-mul+ #x12)
(defconstant +op-div+ #x13)
(defconstant +op-mod+ #x14)
(defconstant +op-neg+ #x15)
(defconstant +op-eq+ #x20)
(defconstant +op-lt+ #x21)
(defconstant +op-gt+ #x22)
(defconstant +op-le+ #x23)
(defconstant +op-ge+ #x24)
(defconstant +op-not+ #x25)
(defconstant +op-car+ #x30)
(defconstant +op-cdr+ #x31)
(defconstant +op-cons+ #x32)
(defconstant +op-ldv+ #x33)
(defconstant +op-vref+ #x34)
(defconstant +op-vstor+ #x35)
(defconstant +op-mkv+ #x36)
(defconstant +op-len+ #x37)
(defconstant +op-ld+ #x40)
(defconstant +op-st+ #x41)
(defconstant +op-args+ #x42)
(defconstant +op-ldg+ #x43)
(defconstant +op-stg+ #x44)
(defconstant +op-sel+ #x50)
(defconstant +op-join+ #x51)
(defconstant +op-loop+ #x52)
(defconstant +op-brk+ #x53)
(defconstant +op-jmp+ #x54)
(defconstant +op-app+ #x60)
(defconstant +op-rtn+ #x61)
(defconstant +op-call+ #x62)
(defconstant +op-prim+ #x70)
(defconstant +op-prn+ #x78)
(defconstant +op-dump+ #x79)
(defconstant +op-gc+ #x7a)
(defconstant +op-nop+ #x7b)
(defconstant +op-pop+ #x7c)
(defconstant +op-error+ #xff)

;; Pool footer magic (must match SECD_POOL_MAGIC in the C runtime)
(defconstant +pool-magic+ #xB1C5)

;;; Emit a byte
(defun emit-byte (context byte)
  "Emit a single byte to the code buffer."
  (vector-push-extend byte (compilation-context-code context)))

;;; Emit a 16-bit value (big-endian)
(defun emit-u16 (context value)
  "Emit a 16-bit unsigned value to the code buffer."
  (emit-byte context (logand (ash value -8) #xff))
  (emit-byte context (logand value #xff)))

;;; Emit an opcode
(defun emit-opcode (context opcode)
  "Emit an opcode to the code buffer."
  (emit-byte context opcode))

;;; Get or create constant index
(defun get-constant-index (context value)
  "Get or create an index for a constant value."
  (or (gethash value (compilation-context-constants context))
      (let ((index (length (compilation-context-constants-list context))))
        (setf (gethash value (compilation-context-constants context)) index)
        (push value (compilation-context-constants-list context))
        index)))

;;; Get or create the ROM pool slot for a byte-vector literal (a list of byte
;;; values). Deduplicates identical literals by value.
(defun get-bytevec-slot (context bytes)
  "Return the ROM pool slot for byte-vector literal BYTES, adding it if new."
  (or (gethash bytes (compilation-context-bytevec-slots context))
      (let ((slot (length (compilation-context-bytevec-pool context))))
        (setf (gethash bytes (compilation-context-bytevec-slots context)) slot)
        (setf (compilation-context-bytevec-pool context)
              (nconc (compilation-context-bytevec-pool context) (list bytes)))
        slot)))

;;; Emit OP_LDV for a byte-vector literal
(defun emit-bytevec-literal (context bytes)
  "Emit LDV for byte-vector literal BYTES and return its pool slot."
  (let ((slot (get-bytevec-slot context bytes)))
     (emit-opcode context +op-ldv+)
     (emit-u16 context slot)
     slot))

;;; Encode a host string to a list of UTF-8 byte values (0..255).
;;; Sources are assumed to be UTF-8, so we encode code points, not raw chars.
(defun utf8-bytes (str)
  "Encode STR (a Common Lisp string) to a list of UTF-8 byte values."
  (let ((out '()))
    (loop for ch across str
          for cp = (char-code ch)
          do (cond
               ((< cp #x80)
                (push cp out))
               ((< cp #x800)
                (push (logior #xC0 (ldb (byte 5 6) cp)) out)
                (push (logior #x80 (logand #x3F cp)) out))
               ((< cp #x10000)
                (push (logior #xE0 (ldb (byte 4 12) cp)) out)
                (push (logior #x80 (ldb (byte 6 6) cp)) out)
                (push (logior #x80 (logand #x3F cp)) out))
               (t
                (push (logior #xF0 (ldb (byte 3 18) cp)) out)
                (push (logior #x80 (ldb (byte 6 12) cp)) out)
                (push (logior #x80 (ldb (byte 6 6) cp)) out)
                (push (logior #x80 (logand #x3F cp)) out))))
    (nreverse out)))

;;; Append the ROM byte-vector pool to the code buffer, after OP_STOP.
;;; Layout (big-endian), matching the C loader in bytecode.cpp:
;;;   [u16 count]
;;;   count * (u16 offset, u16 len)   -- offset relative to first data byte
;;;   [byte blobs]
;;;   [u16 pool_size]                 -- bytes of everything above
;;;   [u16 magic 0xB1C5]
(defun emit-bytevec-pool (context)
  "Append the collected byte-vector pool and footer to the code buffer."
  (let ((pool (compilation-context-bytevec-pool context)))
    (when pool
      (emit-u16 context (length pool))
      ;; Directory: offsets are relative to the first data byte
      (let ((total 0))
        (dolist (bytes pool)
          (incf total (length bytes)))
        (let ((offset 0))
          (dolist (bytes pool)
            (emit-u16 context offset)
            (emit-u16 context (length bytes))
            (incf offset (length bytes)))
          ;; Data blobs
          (dolist (bytes pool)
            (dolist (b bytes)
              (emit-byte context b)))
          ;; Footer
          (emit-u16 context (+ 2 (* 4 (length pool)) total))
          (emit-u16 context +pool-magic+))))))

;;; Compile a literal value
(defun compile-literal (context value)
  "Compile a literal value."
  (cond
    ((integerp value)
     (emit-opcode context +op-ldc+)
     (emit-u16 context (logand value #xffff)))
    ((vectorp value)
     ;; Byte-vector literal (list of bytes). Guard against strings.
     (when (and (vectorp value)
                (not (every #'integerp (coerce value 'list))))
       (error "Cannot compile literal: ~A" value))
     (emit-bytevec-literal context (coerce value 'list)))
    ((eq value t)
     (emit-opcode context +op-ldc+)
     (emit-u16 context 1)) ; True is 1
    ((null value)
     (emit-opcode context +op-ldc+)
     (emit-u16 context 0)) ; Nil is 0
    (t
     (error "Cannot compile literal: ~A" value))))

;;; Resolve a bound variable name to its environment index
(defun variable-index (context name)
  "Return the environment index for a lexically bound NAME, or NIL if not bound."
  (let ((running 0))  ; sizes of frames closer to the innermost scope
    (dolist (frame (compilation-context-scopes context))
      (let ((pos (position name frame)))
        (when pos
          (return-from variable-index (+ pos running))))
      (incf running (length frame)))
    nil))

;;; Packages
;;;
;;; The default package is SECD and (defun main ()) in it is the program's
;;; entry point. Names resolve in the current package: a bare name becomes
;;; CURRENT:name. A fully qualified reference "pkg:name" is only usable once
;;; the package has been loaded into the current package (see below).
;;; Built-in opcodes, primitives, core functions, keywords and t/nil are
;;; global (reachable from any package).
;;;
;;; Libraries are pulled in with:
;;;     (require :ws2812)                        ; ws2812:name usable
;;;     (require :ws2812 :refer (rgb-off))       ; rgb-off -> ws2812:rgb-off
;;;     (require :ws2812 :refer ((rgb-off off))) ; off -> ws2812:rgb-off
;;;     (load "ws2812")                          ; load without refer
;;; The same options can be written as (:require :ws2812 :refer ...) inside a
;;; defpackage. There is no :use.

;;; Operator value of an application node, if its operator is a symbol or a
;;; keyword (the parser represents both as :symbol nodes).
(defun application-operator (node)
  "Return NODE's operator value when NODE is an application, else NIL."
  (when (ast-application-p node)
    (let ((op (ast-node-value node)))
      (when (ast-symbol-p op) (ast-node-value op)))))

;;; Check if a node is a package directive (in-package / defpackage)
(defun package-directive-name (node)
  "Return the directive name (e.g. \"IN-PACKAGE\") if NODE is one, else NIL."
  (let ((op (application-operator node)))
    (when (and op (symbolp op)
               (member (symbol-name op) '("IN-PACKAGE" "DEFPACKAGE")
                       :test #'string-equal))
      (symbol-name op))))

(defun package-directive-p (node)
  "Check if NODE is an in-package or defpackage directive."
  (not (null (package-directive-name node))))

;;; Resolve a symbol name to its package-qualified form
(defun canonical-sym (context name)
  "Resolve NAME to its fully qualified symbol in the current package."
  (let ((sname (symbol-name name)))
    (cond
      ;; Already qualified (pkg:name / pkg::name), a keyword, t/nil, or a
      ;; symbol from another package (CL/alexandria exports leak into the
      ;; SECD-LISP package via :use; they are treated as global)
      ((or (find #\: sname)
           (keywordp name)
           (not (eq (symbol-package name) (find-package "SECD-LISP")))
           (or (string= sname "T") (string= sname "NIL")))
       name)
      (t
       (intern (concatenate 'string (compilation-context-current-package context)
                            ":" sname)
               "SECD-LISP")))))

;;; Library spec for (require :lib ...) / (load "path ..."): the value of the
;;; first child, lowercased so it works as a file path (package names keep
;;; their case; paths do not).
(defun library-spec-path (node)
  "Return the search-path-relative library path named by NODE's first child."
  (let* ((arg (first (ast-node-children node)))
         (value (and arg (ast-node-value arg))))
    (string-downcase (if (symbolp value) (symbol-name value)
                         (princ-to-string value)))))

;;; Library path for a (load "path") directive
(defun load-path (node)
  "Return the library path of a (load ...) node, else NIL."
  (when (and (ast-application-p node)
             (let ((op (application-operator node)))
               (and op (symbolp op)
                    (string-equal (symbol-name op) "LOAD"))))
    (library-spec-path node)))

;;; True if NODE is a (require ...) application
(defun require-node-p (node)
  "Return non-NIL if NODE is a (require ...) directive."
  (and (ast-application-p node)
       (let ((op (application-operator node)))
         (and op (symbolp op)
              (string-equal (symbol-name op) "REQUIRE")))))

;;; One :refer spec into (foreign . local) symbol-name strings.
;;;   (rgb-off)        -> ("RGB-OFF" . "RGB-OFF")
;;;   ((rgb-off off))  -> ("RGB-OFF" . "OFF")
(defun parse-refer-spec (spec)
  "Parse a single :refer spec node into (FOREIGN . LOCAL) name strings."
  (let ((op (ast-node-value spec)))
    (if (ast-symbol-p op)
        (let ((name (symbol-name (ast-node-value op))))
          (cons name name))
        (let* ((foreign (symbol-name (ast-node-value (ast-node-value op))))
               (args (ast-node-children op))
               (local (if args
                          (symbol-name (ast-node-value (first args)))
                          foreign)))
          (cons foreign local)))))

;;; Refer rules of a require lib-spec: from its ":refer <spec>" siblings.
;;;   :refer :all          -> (:all) (expanded to the library's exports)
;;;   :refer (rgb-off)     -> (("RGB-OFF" . "RGB-OFF"))
;;;   :refer ((rgb-off off)) -> (("RGB-OFF" . "OFF"))
(defun parse-refer-rules (node)
  "Return a list of (FOREIGN . LOCAL) rules from NODE's :refer options,
or (:all) when the refer option is :all."
  (loop for rest on (ast-node-children node)
        for child = (first rest)
        when (and (ast-symbol-p child)
                  (keywordp (ast-node-value child))
                  (string-equal (symbol-name (ast-node-value child)) "REFER"))
          append (let ((spec (second rest)))
                   (cond
                     ((null spec) nil)
                     ((and (ast-symbol-p spec)
                           (keywordp (ast-node-value spec))
                           (string-equal (symbol-name (ast-node-value spec)) "ALL"))
                      (list :all))
                     (t (list (parse-refer-spec spec)))))))

;;; Parse one library spec of a require node into (PATH-STRING . RULES).
;;;   :ws2812                -> ("ws2812" . NIL)   ; require, no refer
;;;   (:ws2812 :refer :all)  -> ("ws2812" . (:all))
(defun parse-lib-spec (spec)
  "Parse one library spec into (PATH-STRING . RULES)."
  (cons (string-downcase (symbol-name (if (ast-symbol-p spec)
                                          (ast-node-value spec)
                                          (application-operator spec))))
        (if (ast-symbol-p spec)
            nil
            (parse-refer-rules spec))))

;;; Library specs of a require node
(defun parse-lib-specs (node)
  "Return the library specs of a require node: a list of (PATH . RULES)."
  (mapcar #'parse-lib-spec (ast-node-children node)))

;;; Package descriptors are (REQUIRED REFERS EXPORTS):
;;;   REQUIRED - list of package names loaded into this package
;;;   REFERS   - alist of (LOCAL-NAME . QUALIFIED-SYMBOL) imports/aliases
;;;   EXPORTS  - informational only
(defun make-package-descriptor ()
  "Create an empty package descriptor."
  (list nil nil nil))

(defun package-descriptor (context name)
  "Return (REQUIRED REFERS EXPORTS) for package NAME, or NIL."
  (gethash name (compilation-context-packages context)))

(defun ensure-package (context name)
  "Make sure package NAME has a descriptor."
  (unless (package-descriptor context name)
    (setf (gethash name (compilation-context-packages context))
          (make-package-descriptor))))

(defun package-required (context name)
  "Return the packages loaded into package NAME."
  (first (package-descriptor context name)))

(defun package-refers (context name)
  "Return the (LOCAL . QUALIFIED) alist of package NAME."
  (second (package-descriptor context name)))

;;; Is PKG usable (loaded/required, declared in this program, or the current
;;; package itself) from the current package?
(defun package-accessible-p (context pkg)
  "Return non-NIL if PKG can be referenced from the current package."
  (let ((cur (compilation-context-current-package context)))
    (or (string-equal pkg cur)
        (member pkg (package-required context cur) :test #'string=)
        ;; packages declared in this program (defpackage/in-package) are
        ;; reachable without require; only libraries need require/load
        (not (null (package-descriptor context pkg))))))

;;; Resolve a function name to its canonical defined symbol
(defun resolve-function-name (context name)
  "Return the canonical symbol that NAME resolves to as a function call.
Resolves local definitions, then refers/aliases, then global names."
  (let ((s (symbol-name name)))
    (cond
      ;; Fully qualified (pkg:name): only usable once the package is loaded
      ((find #\: s)
       (let ((pkg (subseq s 0 (position #\: s))))
         (unless (package-accessible-p context pkg)
           (error "Package ~A not loaded; add (require :~A) before referencing ~A"
                  pkg (string-downcase pkg) s))
         name))
      ;; t/nil, keywords, and symbols from other packages (CL/alexandria
      ;; names leak into SECD-LISP via :use) are global
      ((or (string-equal s "T") (string-equal s "NIL")
           (keywordp name)
           (not (eq (symbol-package name) (find-package "SECD-LISP"))))
       name)
      (t
       (let ((pkg (compilation-context-current-package context)))
         ;; 1. local definition
         (let ((canon (intern (concatenate 'string pkg ":" s) "SECD-LISP")))
           (when (gethash canon (compilation-context-functions context))
             (return-from resolve-function-name canon)))
         ;; 2. refer / alias
         (let ((ref (assoc s (package-refers context pkg) :test #'string-equal)))
           (when ref
             (return-from resolve-function-name (cdr ref))))
         ;; 3. global names (primitives and core functions)
         name)))))

;;; Record a loaded library in package PKG and apply refer rules
(defun apply-require (context pkg lib-pkg rules)
  "Record LIB-PKG as required in package PKG and apply refer RULES."
  (ensure-package context pkg)
  (pushnew lib-pkg (first (package-descriptor context pkg)) :test #'string=)
  (labels ((add-rule (foreign local)
             (let ((qualified (intern (concatenate 'string lib-pkg ":" foreign)
                                      "SECD-LISP")))
               (push (cons local qualified)
                     (second (package-descriptor context pkg))))))
    (dolist (rule rules)
      (if (eq rule :all)
          ;; Import every symbol the library exports
          (dolist (export (third (package-descriptor context lib-pkg)))
            (add-rule export export))
          (add-rule (car rule) (cdr rule))))))

;;; Process (:require ...) and (:export ...) options of a package directive
(defun apply-defpackage-options (context pname options visited)
  "Process the require/export options of a defpackage/in-package."
  (dolist (option options)
    (when (ast-application-p option)
      (let ((op (ast-node-value option)))
        (when (ast-symbol-p op)
          (let ((oname (symbol-name (ast-node-value op))))
            (cond
              ((string-equal oname "REQUIRE")
               (dolist (spec (parse-lib-specs option))
                 (let* ((path (car spec))
                        (rules (cdr spec))
                        (lib-pkg (load-library-file
                                  context
                                  (resolve-library-file path) visited)))
                   (apply-require context pname lib-pkg rules))))
              ((string-equal oname "EXPORT")
               ;; Record the exported names so (:refer :all) can import them
               (setf (third (package-descriptor context pname))
                     (union (mapcar (lambda (child)
                                      (symbol-name (ast-node-value child)))
                                    (ast-node-children option))
                            (third (package-descriptor context pname))
                            :test #'string-equal)))
              (t nil))))))))

;;; Handle an in-package / defpackage top-level directive
(defun compile-package-directive (context node &optional visited)
  "Compile a package directive (in-package / defpackage). Emits no code."
  (let* ((dname (package-directive-name node))
         (args (ast-node-children node)))
    (cond
      ((string-equal dname "IN-PACKAGE")
       (let ((pname (string-upcase (ast-node-value (first args)))))
         (ensure-package context pname)
         (apply-defpackage-options context pname (rest args) visited)
         (setf (compilation-context-current-package context) pname)))
      ((string-equal dname "DEFPACKAGE")
       (let ((pname (string-upcase (ast-node-value (first args)))))
         (ensure-package context pname)
         (apply-defpackage-options context pname (rest args) visited)
         ;; One file = one package: defpackage alone establishes the current
         ;; package, so an explicit (in-package ...) is not needed.
         (setf (compilation-context-current-package context) pname)))
      (t nil))))


;;; Compile a variable reference
(defun compile-variable (context name)
  "Compile a variable reference."
  (let ((s (symbol-name name)))
    (when (find #\: s)
      (let ((pkg (subseq s 0 (position #\: s))))
        (unless (package-accessible-p context pkg)
          (error "Package ~A not loaded; add (require :~A) before referencing ~A"
                 pkg (string-downcase pkg) s))))
    (let* ((canon (canonical-sym context name))
           (idx (variable-index context canon)))
      (if idx
          (progn
            (emit-opcode context +op-ld+)
            (emit-u16 context idx))
          ;; Module-level constant: splice the literal value
          (let ((const (gethash canon (compilation-context-top-level-constants context))))
            (if const
                (compile-literal context const)
                ;; Resolve refers/aliases, then check for a defvar'd global.
                (let* ((pkg (compilation-context-current-package context))
                       (ref (unless (find #\: s)
                              (assoc s (package-refers context pkg) :test #'string-equal)))
                       (resolved (if ref (cdr ref) canon))
                       (gslot (gethash resolved (compilation-context-top-level-variables context))))
                  (if gslot
                      (progn
                        (emit-opcode context +op-ldg+)
                        (emit-u16 context gslot))
                      ;; Undefined symbol: this is a hard compile error, not
                      ;; a placeholder constant.
                      (error "Undefined symbol: ~A" s)))))))))

;;; Symbols compiled directly to VM opcodes
(defun builtin-opcode-p (name)
  "Check if a symbol is a builtin VM opcode."
  (member name '(+ - * / mod neg = < > <= >= not car cdr cons print gc
                  vref length make-vector)
          :test #'eq))

;;; USB primitives -> the USB device class they require. Used to raise a
;;; descriptive compile error when a target cannot supply the class (e.g.
;;; %hid-key on an ESP32-C3, whose USB has no HID).
(defparameter *usb-class-map*
  '(("%hid-key" . "hid")
    ("%hid-mouse" . "hid")
    ("%usb-hid-add" . "hid")
    ("%usb-mouse-add" . "hid")
    ("%usb-serial-add" . "serial")
    ("%serial-write" . "serial")
    ("%serial-read" . "serial")
    ("%serial-avail" . "serial"))
  "Primitive name -> USB device class it requires.")

(defun ensure-usb-class (name)
  "If NAME is a known USB primitive the current target cannot provide,
raise a descriptive error."
  (let ((req (assoc (symbol-name name) *usb-class-map* :test #'string-equal)))
    (when req
      (unless (usb-class-device-p (cdr req))
        (error "~S is not supported on this target (~A):~%  ~A~%Choose a target whose USB controller provides that class (e.g. rp2040*)."
               name (and *target* (target-name *target*))
               (usb-note))))))

;;; Compile the target (operator) of a function or primitive call.
;;; Returns (values kind target resolved) where kind is :func, :prim, or
;;; :lambda.
;;;   :func  - user function, target = bytecode address (emit OP_CALL)
;;;   :prim  - primitive, target = primitive id (emit LDC then OP_APP)
;;;   :lambda - operator was an expression (closure already on stack, emit OP_APP)
;;; RESOLVED is the canonical symbol for symbol operators (used for primitive
;;; arity lookup); NIL for non-symbol operators.
(defun compile-call-target (context operator)
  "Compile the function/primitive target of a call."
  (if (ast-symbol-p operator)
      (let* ((name (ast-node-value operator))
             (resolved (resolve-function-name context name))
             (fn-addr (gethash resolved (compilation-context-functions context)))
             (prim-id (gethash resolved (compilation-context-primitives context))))
        (cond
          (fn-addr
           (values :func fn-addr resolved))
          ((not (null prim-id))
           (values :prim prim-id resolved))
          ((eq name 't) (compile-literal context t) (values :lambda nil nil))
          ((eq name 'nil) (compile-literal context nil) (values :lambda nil nil))
          (t
           (ensure-usb-class name)
           (error "Unknown function: ~A" name))))
      ;; Non-symbol operator (e.g. lambda expression): compile it
      (progn
        (compile-node context operator)
        (values :lambda nil nil))))

;;; Compile operands into an argument list on the stack:
;;; pushes a1 .. an then LDC nil + Nx CONS -> (a1 ... an) on top.
(defun compile-args-list (context operands)
  "Compile operands and build them into an argument list."
  (dolist (operand operands)
    (compile-node context operand))
  (emit-opcode context +op-ldc+)
  (emit-u16 context 0)  ; nil
  (dotimes (i (length operands))
    (emit-opcode context +op-cons+)))

;;; Board-independent UNIVERSAL runtime primitive metadata.
;;;
;;; These ids (core VM ops 0-14 plus universal software primitives such as
;;; utf16-enc/utf16-dec at 200/201) are identical in EVERY firmware image, so
;;; a program compiled against them is portable across all targets. They live
;;; in targets/machine-runtime.json, separate from the per-chip HAL metadata,
;;; so that .machine / chip JSON stay HAL-only.
(defvar *runtime-metadata-path*
  (merge-pathnames #p"../targets/machine-runtime.json"
                   (first *machine-search-paths*))
  "Path to the board-independent universal runtime primitive metadata.")

(defvar *runtime-primitives* nil
  "Cached hash-table (name -> primitive def) loaded from
  *runtime-metadata-path*. Lazily loaded by RUNTIME-PRIMITIVES.")

(defun runtime-primitives ()
  "Return the universal runtime primitive table, loading it on first use."
  (unless *runtime-primitives*
    (let ((path *runtime-metadata-path*))
      (unless (probe-file path)
        (error "Universal runtime metadata not found at ~A" path))
      (setf *runtime-primitives*
            (gethash "primitives"
                     (yason:parse (uiop:read-file-string path))))))
  *runtime-primitives*)

;;; Populate the primitive table from (a) the universal runtime metadata and
;;; (b) the target's HAL metadata.
(defun load-target-primitives (context target-name)
  "Load primitive ids into the compilation context.

Universal runtime primitives (ids 0-14 and the fixed high ids such as
utf16-enc/utf16-dec = 200/201) come from machine-runtime.json and are
identical on every target. Board-specific HAL primitives (ids >= 15) come
from the target's .machine metadata (the per-chip JSON)."
  (let ((target (load-target target-name)))
    (loop for prim-name being the hash-keys of (runtime-primitives)
            using (hash-value prim-def)
          do (setf (gethash (intern (string-upcase prim-name) "SECD-LISP")
                            (compilation-context-primitives context))
                   (gethash "id" prim-def)))
    (loop for prim-name being the hash-keys of (target-primitives target)
            using (hash-value prim-def)
          do (setf (gethash (intern (string-upcase prim-name) "SECD-LISP")
                            (compilation-context-primitives context))
                   (gethash "id" prim-def)))))

;;; Libraries
;;;
;;; Library files live in *library-search-paths* (default: library/ under the
;;; system, plus any directory added with --lib or the SECD_LIB environment
;;; variable). (require :name) and (load "name") both parse and process a
;;; library file's top-level forms (its defpackage/in-package/require/load
;;; and defuns) at compile time; require additionally makes the library's
;;; symbols referable from the current package (qualified, or via :refer).

;;; Search paths for library files
(defvar *library-search-paths*
  (list (asdf:system-relative-pathname :secd-lisp "library/"))
  "Directories searched for library files.")

;;; Add a directory to the library search path
(defun add-library-search-path (path)
  "Add PATH to the library search path."
  (push (uiop:parse-native-namestring path) *library-search-paths*))

;;; Resolve a library file by name
(defun resolve-library-file (name)
  "Find the .lisp file for library NAME in the search paths."
  (let ((filename (format nil "~A.lisp" name)))
    (or (loop for path in *library-search-paths*
              for file = (merge-pathnames filename path)
              when (probe-file file)
                return file)
        (error "Library file not found: ~A (searched: ~{~A~^, ~})"
               name *library-search-paths*))))

;;; Package a library file declares, if any
(defun library-package-name (program)
  "Return the package name declared by the library's first directive."
  (let ((forms (ast-node-value program)))
    (or (loop for form in forms
              when (and (package-directive-p form)
                        (string-equal (package-directive-name form) "DEFPACKAGE"))
                return (string-upcase (ast-node-value (first (ast-node-children form)))))
        (loop for form in forms
              when (and (package-directive-p form)
                        (string-equal (package-directive-name form) "IN-PACKAGE"))
                return (string-upcase (ast-node-value (first (ast-node-children form))))))))

;;; Load a library file, processing its forms with its own package in scope
(defun load-library-file (context path visited)
  "Parse and process the top-level forms of library file PATH."
  (when (member path visited :test #'equal)
    (error "Circular require/load: ~A" path))
  (let* ((lib-ast (parse (tokenize (uiop:read-file-string path))))
         (lib-pkg (library-package-name lib-ast))
         (saved (compilation-context-current-package context)))
    (process-top-level-forms context (ast-node-value lib-ast) (cons path visited))
    (setf (compilation-context-current-package context) saved)
    (or lib-pkg (string-upcase (pathname-name path)))))

;;; Compile a top-level (require libspec*) directive
(defun compile-require-directive (context node visited)
  "Process a top-level (require ...) directive with one or more lib specs."
  (dolist (spec (parse-lib-specs node))
    (let* ((path (car spec))
           (rules (cdr spec))
           (lib-pkg (load-library-file context
                                      (resolve-library-file path) visited)))
      (apply-require context (compilation-context-current-package context)
                     lib-pkg rules))))

;;; Compile a top-level (load "path") directive
(defun compile-load-directive (context node visited)
  "Process a (load \"path\") directive (a require without refer)."
  (let* ((path (resolve-library-file (load-path node)))
         (lib-pkg (load-library-file context path visited)))
    (apply-require context (compilation-context-current-package context)
                   lib-pkg nil)))

;;; Record a module-level (defconstant NAME <literal>) as a constant;
;;; references to NAME are spliced in as the literal value. Emits no runtime
;;; code. (defvar, by contrast, declares a variable and is not a constant.)
(defun compile-defconstant (context node)
  "Record a module-level (defconstant NAME <literal>) as a constant."
  (let* ((canon (canonical-sym context (ast-node-value node)))
         (value-node (first (ast-node-children node)))
         (value (cond
                  ((ast-integer-p value-node) (ast-node-value value-node))
                  ((eq (ast-node-type value-node) :boolean)
                   (ast-node-value value-node))
                  ((eq (ast-node-type value-node) :nil) nil)
                  ((ast-byte-vector-p value-node)
                   (coerce (ast-node-value value-node) 'vector))
                  ((ast-symbol-p value-node)
                   (let ((v (ast-node-value value-node)))
                     (cond ((eq v 't) t)
                           ((eq v 'nil) nil)
                           (t (error "defconstant value must be a constant literal, got ~S"
                                     v)))))
                  (t (error "defconstant value must be a constant literal, got ~S"
                            (ast-node-type value-node))))))
    (setf (gethash canon (compilation-context-top-level-constants context)) value)))

;;; Compile a top-level (defvar NAME [value]). Registers NAME as a global
;;; variable cell; the initializer is emitted at program entry (before main).
(defun compile-defvar (context node)
  (let* ((name (ast-node-value node))
         (canon (canonical-sym context name)))
    (when (gethash canon (compilation-context-top-level-constants context))
      (error "Cannot defvar ~A: already a defconstant" name))
    (unless (gethash canon (compilation-context-top-level-variables context))
      (setf (gethash canon (compilation-context-top-level-variables context))
            (length (compilation-context-top-level-variable-order context)))
      (push canon (compilation-context-top-level-variable-order context))
      (setf (gethash canon (compilation-context-top-level-variable-inits context))
            (first (ast-node-children node))))))

;;; Emit the global-frame initialization at program entry: for each defvar'd
;;; global in declaration order, push its initializer (or NIL) and STG it.
(defun emit-global-inits (context)
  (dolist (canon (reverse (compilation-context-top-level-variable-order context)))
    (let* ((init (gethash canon (compilation-context-top-level-variable-inits context)))
           (slot (gethash canon (compilation-context-top-level-variables context))))
      (if init
          (compile-node context init)
          (compile-literal context nil))
      (emit-opcode context +op-stg+)
      (emit-u16 context slot))))

;;; Process top-level forms in source order: package directives, require/load
;;; and defuns. Only these are allowed at the top level; the program entry
;;; point is always (defun main ()) in the SECD package.
(defun process-top-level-forms (context forms visited)
  "Process the top-level FORMS of a program or library file."
  (dolist (form forms)
    (cond
      ((package-directive-p form)
       (compile-package-directive context form visited))
      ((require-node-p form)
       (compile-require-directive context form visited))
      ((load-path form)
       (compile-load-directive context form visited))
       ((eq (ast-node-type form) :defun)
        (compile-node context form))
       ((eq (ast-node-type form) :def-c-fun)
        (compile-node context form))
       ((eq (ast-node-type form) :defconstant)
        (compile-defconstant context form))
       ((eq (ast-node-type form) :defvar)
        (compile-defvar context form))
       (t
        (error "Unexpected top-level form (~A); only defun, def-c-fun, defpackage, in-package, require, load, defconstant and defvar are allowed"
               (ast-node-type form))))))

;;; Compile an AST node
(defun compile-node (context node)
  "Compile an AST node to bytecode."
  (if (package-directive-p node)
      (compile-package-directive context node)
      (case (ast-node-type node)
    ;; Program (top-level)
    ;; Layout: [JMP <entry>] [defun blobs (skipped at runtime)] [entry]
    ;; Defun blobs are compiled first so the entry can forward-reference them;
    ;; a leading JMP skips the defun region at startup. The entry is always
    ;; a call to (secd:main).
    (:program
     (let ((children (ast-node-value node))
           (entry-start 0))
       ;; Emit JMP placeholder (patched below to skip the defun region)
       (emit-opcode context +op-jmp+)
       (emit-u16 context 0)
       ;; Compile all top-level forms in order (defuns, packages, require/load)
       (process-top-level-forms context children nil)
       ;; Record where the entry code begins
       (setf entry-start (length (compilation-context-code context)))
       ;; Initialize defvar'd globals before calling the entry point
       (emit-global-inits context)
       ;; Entry point: the function chosen at build time (:entry "PKG:FN",
       ;; default SECD:MAIN). A program that declares its own package sets
       ;; the entry to its package's main, e.g. :entry "rgb-blink:main".
        (let ((main-sym (intern (string-upcase (compilation-context-entry context))
                                "SECD-LISP")))
         (unless (gethash main-sym (compilation-context-functions context))
           (error "No (defun ...) for entry point ~A defined"
                  (compilation-context-entry context)))
         (compile-node context
                       (make-application-node
                        (make-symbol-node main-sym) nil)))
        ;; Patch the JMP target to skip the defun region
        (let ((code (compilation-context-code context)))
          (setf (aref code 1) (logand (ash entry-start -8) #xff))
          (setf (aref code 2) (logand entry-start #xff)))))
    
    ;; Integer literal
    (:integer
     (compile-literal context (ast-node-value node)))
    
    ;; Byte-vector literal #( ... )
    (:byte-vector
     (emit-bytevec-literal context (ast-node-value node)))

    ;; String literal "..." — emitted as a UTF-8 byte-vector (a sequence of
    ;; bytes; vref/length operate at byte level, ASCII == character level).
    (:string
     (emit-bytevec-literal context (utf8-bytes (ast-node-value node))))
    
    ;; Boolean literal
    (:boolean
     (compile-literal context (ast-node-value node)))
    
    ;; Nil literal
    (:nil
     (compile-literal context nil))
    
    ;; Symbol reference
    (:symbol
     (let ((name (ast-node-value node)))
       (cond
         ;; Keyword literals (lexer interns keywords case-sensitively)
         ((and (keywordp name) (string= (string-upcase (symbol-name name)) "OUTPUT"))
          (compile-literal context 1))
         ((and (keywordp name) (string= (string-upcase (symbol-name name)) "INPUT"))
          (compile-literal context 0))
         ((keywordp name)
          (error "Unsupported keyword: ~A" name))
         ;; T and nil
         ((eq name 't) (compile-literal context t))
         ((eq name 'nil) (compile-literal context nil))
         ;; Arithmetic operations
         ((eq name '+) (emit-opcode context +op-add+))
         ((eq name '-) (emit-opcode context +op-sub+))
         ((eq name '*) (emit-opcode context +op-mul+))
         ((eq name '/) (emit-opcode context +op-div+))
         ((eq name 'mod) (emit-opcode context +op-mod+))
         ((eq name 'neg) (emit-opcode context +op-neg+))
         ;; Comparison operations
         ((eq name '=) (emit-opcode context +op-eq+))
         ((eq name '<) (emit-opcode context +op-lt+))
         ((eq name '>) (emit-opcode context +op-gt+))
         ((eq name '<=) (emit-opcode context +op-le+))
         ((eq name '>=) (emit-opcode context +op-ge+))
         ;; List operations
         ((eq name 'car) (emit-opcode context +op-car+))
         ((eq name 'cdr) (emit-opcode context +op-cdr+))
         ((eq name 'cons) (emit-opcode context +op-cons+))
         ;; Byte-vector operations
         ((eq name 'vref) (emit-opcode context +op-vref+))
         ((eq name 'length) (emit-opcode context +op-len+))
         ((eq name 'make-vector) (emit-opcode context +op-mkv+))
         ;; Boolean operations
         ((eq name 'not) (emit-opcode context +op-not+))
         ;; I/O operations
         ((eq name 'print) (emit-opcode context +op-prn+))
         ;; GC
         ((eq name 'gc) (emit-opcode context +op-gc+))
         ;; Variable reference
         (t (compile-variable context name)))))
    
    ;; Application
    (:application
     (let ((operator (ast-node-value node))
           (operands (ast-node-children node)))
       (if (and (ast-symbol-p operator)
                (builtin-opcode-p (ast-node-value operator)))
           ;; Builtin opcode: operands then opcode (no APP)
           (progn
             (dolist (operand operands)
               (compile-node context operand))
             (compile-node context operator))
;; Function/primitive/lambda call
            (multiple-value-bind (kind target resolved)
                (compile-call-target context operator)
              (when (eq kind :prim)
                (let ((arity (and resolved
                                  (gethash resolved (compilation-context-prim-arities context)))))
                  (when (and arity (/= arity (length operands)))
                    (error "~A expects ~D argument~:P, but got ~D"
                           resolved arity (length operands))))
                (emit-opcode context +op-ldc+)
                (emit-u16 context target))
              (compile-args-list context operands)
              (case kind
                (:func
                 (emit-opcode context +op-call+)
                 (emit-u16 context target))
                (t
                 (emit-opcode context +op-app+)))))))
    
    ;; Lambda
    (:lambda
     (let ((params (mapcar (lambda (p) (canonical-sym context p))
                           (ast-node-value node)))
           (body (ast-node-children node)))
       ;; Emit LDF with placeholder address
       (emit-opcode context +op-ldf+)
       (let ((addr-pos (length (compilation-context-code context))))
         (emit-u16 context 0) ; Placeholder
         ;; Remember where this lambda's code will be
         (push (cons addr-pos (length (compilation-context-code context)))
               (compilation-context-lambdas context))
         ;; Bind parameters: pop params off the S stack into the environment
         (emit-opcode context +op-args+)
         (emit-byte context (length params))
         ;; Push a scope frame for the parameters
         (push (copy-list params) (compilation-context-scopes context))
         ;; Compile body (discard intermediate results)
         (loop for rest on body
               for expr = (first rest)
               do (compile-node context expr)
                  (unless (null (rest rest))
                    (emit-opcode context +op-pop+)))
         ;; Pop the parameter scope frame
         (pop (compilation-context-scopes context))
         ;; Emit RTN
         (emit-opcode context +op-rtn+)
         ;; Update the address to point at the function body
         (let ((code-vec (compilation-context-code context)))
           (setf (aref code-vec addr-pos)
                 (logand (ash (+ addr-pos 2) -8) #xff))
           (setf (aref code-vec (1+ addr-pos))
                 (logand (+ addr-pos 2) #xff))))))
    
    ;; Defun
     (:defun
      (let* ((name-and-params (ast-node-value node))
             (name (canonical-sym context (car name-and-params)))
             (params (second name-and-params))
             (body (ast-node-children node))
             ;; LDF opcode + 2 operand bytes precede the function body
             (body-start (+ (length (compilation-context-code context)) 3)))
        ;; Register the name before compiling the body so a function can
        ;; recurse on itself.
        (setf (gethash name (compilation-context-functions context)) body-start)
        ;; Also register under the current package's qualified name so the
        ;; function is reachable as PKG:NAME even when NAME collides with a
        ;; CL/global symbol (e.g. (defun count ...) canonicalizes to
        ;; CL:COUNT; the qualified form is what a library user references).
        (let ((qname (intern (concatenate 'string
                                          (compilation-context-current-package context)
                                          ":" (symbol-name (car name-and-params)))
                             "SECD-LISP")))
          (unless (eq qname name)
            (setf (gethash qname (compilation-context-functions context)) body-start)))
        ;; Compile as lambda
        (compile-node context (make-lambda-node params body))))

     ;; Def-c-fun: bind LISP-NAME to an existing C function C-FUN-NAME as a
     ;; primitive alias. Emits no code; registers the alias in the primitive
     ;; table (both unqualified and package-qualified) so it is callable bare
     ;; or via a :refer rule. Only non-HAL (software) C functions are allowed.
     (:def-c-fun
      (let* ((value (ast-node-value node))
             (lisp-name (first value))
             (c-fun-name (second value))
             (params (third value))
             (c-fun-sym (intern (string-upcase (symbol-name c-fun-name)) "SECD-LISP"))
             (id (gethash c-fun-sym (compilation-context-primitives context))))
        (when (and (> (length (symbol-name c-fun-name)) 0)
                   (char= (char (symbol-name c-fun-name) 0) #\%))
          (error "def-c-fun cannot bind to HAL primitive ~A; call the %-primitive directly"
                 c-fun-name))
        (unless id
          (error "def-c-fun: unknown C function ~A (not present in the target's runtime)"
                 c-fun-name))
        (let ((arity (length params))
              (unq (intern (string-upcase (symbol-name lisp-name)) "SECD-LISP"))
              (qual (intern (concatenate 'string
                                         (compilation-context-current-package context)
                                         ":" (string-upcase (symbol-name lisp-name)))
                            "SECD-LISP")))
          (setf (gethash unq (compilation-context-primitives context)) id)
          (setf (gethash qual (compilation-context-primitives context)) id)
          (setf (gethash unq (compilation-context-prim-arities context)) arity)
          (setf (gethash qual (compilation-context-prim-arities context)) arity)))
      (values))

    
    ;; If
    (:if
     (let ((children (ast-node-children node)))
       (when (/= (length children) 3)
         (error "if requires exactly 3 arguments"))
       ;; Compile condition
       (compile-node context (first children))
       ;; SEL <else_addr>: cond false jumps to else; true falls through.
       (emit-opcode context +op-sel+)
       (let ((else-pos (length (compilation-context-code context))))
         (emit-u16 context 0)              ; placeholder for else_addr
         ;; Then branch (inline, executed when cond TRUE)
         (compile-node context (second children))
         ;; Keep then value on stack, skip the else branch
         (emit-opcode context +op-jmp+)
         (let ((post-pos (length (compilation-context-code context))))
           (emit-u16 context 0)           ;; placeholder for post
           ;; Else address = first byte of the else branch
           (let ((else-addr (length (compilation-context-code context))))
             ;; Else branch (executed when cond FALSE)
             (compile-node context (third children))
             ;; Post = first byte after the whole conditional
             (let ((post (length (compilation-context-code context))))
               (setf (aref (compilation-context-code context) else-pos)
                     (logand (ash else-addr -8) #xff))
               (setf (aref (compilation-context-code context) (1+ else-pos))
                     (logand else-addr #xff))
               (setf (aref (compilation-context-code context) post-pos)
                     (logand (ash post -8) #xff))
               (setf (aref (compilation-context-code context) (1+ post-pos))
                     (logand post #xff))))))))
    
    ;; When (if with implicit nil else)
    (:when
     (let ((children (ast-node-children node)))
       (when (< (length children) 1)
         (error "when requires at least 1 argument"))
       ;; Compile condition
       (compile-node context (first children))
       ;; Emit conditional branch (false jumps to else)
       (emit-opcode context +op-sel+)
       (let ((else-pos (length (compilation-context-code context))))
         (emit-u16 context 0) ; Placeholder for else address
         ;; Then body (executed when cond TRUE)
         (dolist (expr (rest children))
           (compile-node context expr))
         (emit-opcode context +op-jmp+)
         (let ((post-pos (length (compilation-context-code context))))
           (emit-u16 context 0) ;; Placeholder for post
           ;; Else branch: push nil
           (let ((else-addr (length (compilation-context-code context))))
             (compile-literal context nil)
             (let ((post (length (compilation-context-code context))))
               (setf (aref (compilation-context-code context) else-pos)
                     (logand (ash else-addr -8) #xff))
               (setf (aref (compilation-context-code context) (1+ else-pos))
                     (logand else-addr #xff))
               (setf (aref (compilation-context-code context) post-pos)
                     (logand (ash post -8) #xff))
               (setf (aref (compilation-context-code context) (1+ post-pos))
                     (logand post #xff))))))))
    
    ;; Unless (if with implicit nil then)
    (:unless
     (let ((children (ast-node-children node)))
       (when (< (length children) 1)
         (error "unless requires at least 1 argument"))
       ;; Compile condition
       (compile-node context (first children))
       ;; Emit NOT
       (emit-opcode context +op-not+)
       ;; Emit conditional branch (false jumps to else)
       (emit-opcode context +op-sel+)
       (let ((else-pos (length (compilation-context-code context))))
         (emit-u16 context 0) ; Placeholder for else address
         ;; Then body (executed when cond TRUE)
         (dolist (expr (rest children))
           (compile-node context expr))
         (emit-opcode context +op-jmp+)
         (let ((post-pos (length (compilation-context-code context))))
           (emit-u16 context 0) ;; Placeholder for post
           ;; Else branch: push nil
           (let ((else-addr (length (compilation-context-code context))))
             (compile-literal context nil)
             (let ((post (length (compilation-context-code context))))
               (setf (aref (compilation-context-code context) else-pos)
                     (logand (ash else-addr -8) #xff))
               (setf (aref (compilation-context-code context) (1+ else-pos))
                     (logand else-addr #xff))
               (setf (aref (compilation-context-code context) post-pos)
                     (logand (ash post -8) #xff))
               (setf (aref (compilation-context-code context) (1+ post-pos))
                     (logand post #xff))))))))
    
    ;; Let
(:let
     (let ((bindings (ast-node-value node))
           (body (ast-node-children node)))
       ;; Compile all init expressions in the enclosing scope (parallel let:
       ;; none of the new bindings are visible while computing inits)
       (dolist (binding bindings)
         (compile-node context (cdr binding))) ; compile value
       ;; Emit ARGS to create the environment
       (emit-opcode context +op-args+)
       (emit-byte context (length bindings))
       ;; Scope frame for the let bindings (order matches ARGS binding order)
       (push (mapcar (lambda (b) (canonical-sym context (car b))) bindings)
             (compilation-context-scopes context))
       ;; Compile body
       (dolist (expr body)
         (compile-node context expr))
       ;; Pop the let scope frame
       (pop (compilation-context-scopes context))))
    
    ;; Let*
    (:let*
     (let ((bindings (ast-node-value node))
           (body (ast-node-children node)))
       ;; Compile bindings one by one
       (dolist (binding bindings)
         (compile-node context (cdr binding)) ; compile value
         ;; Emit ARGS for each binding
         (emit-opcode context +op-args+)
         (emit-byte context 1)
         ;; Each binding is its own scope frame
         (push (list (canonical-sym context (car binding)))
               (compilation-context-scopes context)))
       ;; Compile body
       (dolist (expr body)
         (compile-node context expr))
       ;; Pop the let* scope frames
       (dotimes (i (length bindings))
         (pop (compilation-context-scopes context)))))
    
    ;; Cond
    (:cond
     (let ((clauses (ast-node-children node)))
       (when (null clauses)
         (compile-literal context nil)
         (return-from compile-node))
       ;; Compile first clause. A clause ((test) body...) parses as an
        ;; application node: the test is the operator (ast-node-value) and the
        ;; body forms are the operands (ast-node-children).
        (let ((clause (first clauses)))
          (compile-node context (ast-node-value clause)) ; Condition
          (emit-opcode context +op-sel+)
          (let ((else-pos (length (compilation-context-code context))))
            (emit-u16 context 0) ; Placeholder for else address
            ;; Compile then body (executed when cond TRUE)
            (dolist (expr (ast-node-children clause))
              (compile-node context expr))
           (emit-opcode context +op-jmp+)
           (let ((post-pos (length (compilation-context-code context))))
             (emit-u16 context 0) ;; Placeholder for post
             ;; Else address = start of remaining clauses
             (let ((else-addr (length (compilation-context-code context))))
               ;; Compile remaining clauses (else branch)
               (compile-node context (make-cond-node (rest clauses)))
               (let ((post (length (compilation-context-code context))))
                 (setf (aref (compilation-context-code context) else-pos)
                       (logand (ash else-addr -8) #xff))
                 (setf (aref (compilation-context-code context) (1+ else-pos))
                       (logand else-addr #xff))
                 (setf (aref (compilation-context-code context) post-pos)
                       (logand (ash post -8) #xff))
                 (setf (aref (compilation-context-code context) (1+ post-pos))
                       (logand post #xff)))))))))
    
    ;; Setf
    (:setf
     (let ((place (ast-node-value node))
           (value (first (ast-node-children node))))
       ;; (setf (vref vec idx) value): place is the application node
       ;; (parser keeps the whole place form); plain variable assignment
       ;; stores a raw symbol.
       (if (and (ast-node-p place) (ast-application-p place))
           ;; Compile vec, idx, then value; VSTOR pops (val idx vec) in that
           ;; order, so push vec first, then idx, then the value.
           (progn
             (dolist (operand (ast-node-children place))
               (compile-node context operand))
             (compile-node context value)
             (emit-opcode context +op-vstor+))
;; Plain variable assignment: name is a symbol
            (progn
              (compile-node context value)
              (let* ((pname (symbol-name place))
                     (canon (canonical-sym context place))
                     (idx (variable-index context canon)))
                (if idx
                    (progn
                      (emit-opcode context +op-st+)
                      (emit-u16 context idx))
                    ;; Not a local: it must be a defvar'd global. Implicit
                    ;; globals are not allowed, and constants cannot be setf'd.
                    (let* ((pkg (compilation-context-current-package context))
                           (ref (unless (find #\: pname)
                                  (assoc pname (package-refers context pkg) :test #'string-equal)))
                           (resolved (if ref (cdr ref) canon))
                           (gslot (gethash resolved (compilation-context-top-level-variables context))))
                      (cond
                        (gslot
                         (emit-opcode context +op-stg+)
                         (emit-u16 context gslot))
                        ((gethash resolved (compilation-context-top-level-constants context))
                         (error "Cannot setf constant ~A" place))
                        (t
                         (error "Cannot setf undefined variable ~A" place))))))))))
    
    ;; Progn
    (:progn
     (let ((body (ast-node-children node)))
       (loop for rest on body
             for expr = (first rest)
             do (compile-node context expr)
                (unless (null (rest rest))
                  (emit-opcode context +op-pop+)))))
    
    ;; Quote
    (:quote
     (compile-literal context (ast-node-value node)))

    ;; Loop: execute body forever
    (:loop
     (let ((loop-start (length (compilation-context-code context))))
       (dolist (expr (ast-node-children node))
         (compile-node context expr)
         (emit-opcode context +op-pop+))
       (emit-opcode context +op-loop+)
       (emit-u16 context loop-start)))
    
    ;; Default
    (t
     (error "Unknown AST node type: ~A" (ast-node-type node))))))

;;; Helper to create cond node
(defun make-cond-node (clauses)
  "Create a cond AST node from clauses."
  (make-ast-node :type :cond :value nil :children clauses))

;;; Compile AST to bytecode
(defun compile-to-bytecode (ast &optional (target :rp2040)
                            &key (entry "SECD:MAIN"))
  "Compile an AST to SECD bytecode."
  (let ((context (make-compilation-context)))
    (when target
      (setf *target* (load-target target))
      (load-target-primitives context target))
    (setf (compilation-context-entry context) entry)
    (compile-node context ast)
    (emit-opcode context +op-stop+)
    ;; ROM byte-vector pool + footer (after OP_STOP; see SECD_POOL_MAGIC)
    (emit-bytevec-pool context)
    (compilation-context-code context)))

;;; Compile a file
(defun secd-compile-file (filename &key (target :rp2040) (entry "SECD:MAIN"))
  "Compile a secd-lisp file to SECD bytecode."
  (let* ((source (uiop:read-file-string filename))
         (tokens (tokenize source))
         (ast (parse tokens))
         (bytecode (compile-to-bytecode ast target :entry entry)))
    bytecode))

;;; Compile a string
(defun compile-string (string &key (target :rp2040) (entry "SECD:MAIN"))
  "Compile a secd-lisp string to SECD bytecode."
  (let* ((tokens (tokenize string))
         (ast (parse tokens))
         (bytecode (compile-to-bytecode ast target :entry entry)))
    bytecode))
