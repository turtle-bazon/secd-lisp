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
  (scopes nil :type list))  ; lexical scopes: list of frames; each frame is a list of bound names (innermost frame first)

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
(defconstant +op-ld+ #x40)
(defconstant +op-st+ #x41)
(defconstant +op-args+ #x42)
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

;;; Compile a literal value
(defun compile-literal (context value)
  "Compile a literal value."
  (cond
    ((integerp value)
     (emit-opcode context +op-ldc+)
     (emit-u16 context (logand value #xffff)))
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

;;; Compile a variable reference
(defun compile-variable (context name)
  "Compile a variable reference."
  (let ((idx (variable-index context name)))
    (if idx
        (progn
          (emit-opcode context +op-ld+)
          (emit-u16 context idx))
        ;; Unresolved symbol: keep a placeholder constant reference.
        (progn
          (emit-opcode context +op-ldc+)
          (emit-u16 context (get-constant-index context name))))))

;;; Symbols compiled directly to VM opcodes
(defun builtin-opcode-p (name)
  "Check if a symbol is a builtin VM opcode."
  (member name '(+ - * / mod neg = < > <= >= not car cdr cons print gc)
          :test #'eq))

;;; Compile the target (operator) of a function or primitive call.
;;; Returns (values kind target) where kind is :func, :prim, or :lambda.
;;;   :func  - user function, target = bytecode address (emit OP_CALL)
;;;   :prim  - primitive, target = primitive id (emit LDC then OP_APP)
;;;   :lambda - operator was an expression (closure already on stack, emit OP_APP)
(defun compile-call-target (context operator)
  "Compile the function/primitive target of a call."
  (if (ast-symbol-p operator)
      (let* ((name (ast-node-value operator))
             (fn-addr (gethash name (compilation-context-functions context)))
             (prim-id (gethash name (compilation-context-primitives context))))
        (cond
          (fn-addr
           (values :func fn-addr))
          ((not (null prim-id))
           (values :prim prim-id))
          ((eq name 't) (compile-literal context t) (values :lambda nil))
          ((eq name 'nil) (compile-literal context nil) (values :lambda nil))
          (t (error "Unknown function: ~A" name))))
      ;; Non-symbol operator (e.g. lambda expression): compile it
      (progn
        (compile-node context operator)
        (values :lambda nil))))

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

;;; Populate the primitive table from a target's .machine metadata
(defun load-target-primitives (context target-name)
  "Load target primitive ids into the compilation context."
  (let ((target (load-target target-name)))
    (loop for prim-name being the hash-keys of (target-primitives target)
            using (hash-value prim-def)
          do (setf (gethash (intern (string-upcase prim-name) "SECD-LISP")
                            (compilation-context-primitives context))
                   (gethash "id" prim-def)))))

;;; Compile an AST node
(defun compile-node (context node)
  "Compile an AST node to bytecode."
  (case (ast-node-type node)
    ;; Program (top-level)
    ;; Layout: [JMP <driver>] [defun blobs (skipped at runtime)] [driver]
    ;; Defun bodies are compiled first so the driver can forward-reference
    ;; them; a leading JMP skips the defun region at startup.
    (:program
     (let ((children (ast-node-value node))
           (driver-start 0))
       ;; Emit JMP placeholder (patched below to skip the defun region)
       (emit-opcode context +op-jmp+)
       (emit-u16 context 0)
       ;; Pass 1: compile defun blobs (LDF <body> / body / RTN)
       (dolist (child children)
         (when (eq (ast-node-type child) :defun)
           (compile-node context child)))
       ;; Record where the driver code begins
       (setf driver-start (length (compilation-context-code context)))
       ;; Pass 2: compile remaining top-level forms (the driver)
       (dolist (child children)
         (unless (eq (ast-node-type child) :defun)
           (compile-node context child)))
       ;; Patch the JMP target to skip the defun region
       (let ((code (compilation-context-code context)))
         (setf (aref code 1) (logand (ash driver-start -8) #xff))
         (setf (aref code 2) (logand driver-start #xff)))))
    
    ;; Integer literal
    (:integer
     (compile-literal context (ast-node-value node)))
    
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
           (multiple-value-bind (kind target)
               (compile-call-target context operator)
             (when (eq kind :prim)
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
     (let ((params (ast-node-value node))
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
            (name (car name-and-params))
            (params (second name-and-params))
            (body (ast-node-children node))
            ;; LDF opcode + 2 operand bytes precede the function body
            (body-start (+ (length (compilation-context-code context)) 3)))
       ;; Register the name before compiling the body so a function can
       ;; recurse on itself.
       (setf (gethash name (compilation-context-functions context)) body-start)
       ;; Compile as lambda
       (compile-node context (make-lambda-node params body))))
    
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
     (let ((bindings (first (ast-node-children node)))
           (body (rest (ast-node-children node))))
       ;; Compile bindings
       (dolist (binding bindings)
         (compile-node context (second binding))) ; Compile value
       ;; Emit ARGS to create environment
       (emit-opcode context +op-args+)
       (emit-byte context (length bindings))
       ;; Scope frame for the let bindings (order matches ARGS binding order)
       (push (mapcar #'first bindings) (compilation-context-scopes context))
       ;; Compile body
       (dolist (expr body)
         (compile-node context expr))
       ;; Pop the let scope frame
       (pop (compilation-context-scopes context))))
    
    ;; Let*
    (:let*
     (let ((bindings (first (ast-node-children node)))
           (body (rest (ast-node-children node))))
       ;; Compile bindings one by one
       (dolist (binding bindings)
         (compile-node context (second binding)) ; Compile value
         ;; Emit ARGS for each binding
         (emit-opcode context +op-args+)
         (emit-byte context 1)
         ;; Each binding is its own scope frame
         (push (list (first binding)) (compilation-context-scopes context)))
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
       ;; Compile first clause
       (let ((clause (first clauses)))
         (compile-node context (first clause)) ; Condition
         (emit-opcode context +op-sel+)
         (let ((else-pos (length (compilation-context-code context))))
           (emit-u16 context 0) ; Placeholder for else address
           ;; Compile then body (executed when cond TRUE)
           (dolist (expr (rest clause))
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
     (let ((name (ast-node-value node))
           (value (first (ast-node-children node))))
       ;; Compile value
       (compile-node context value)
       ;; Emit ST to store in environment
       (emit-opcode context +op-st+)
       (let ((idx (variable-index context name)))
         (emit-u16 context (or idx (get-constant-index context name))))))
    
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
     (error "Unknown AST node type: ~A" (ast-node-type node)))))

;;; Helper to create cond node
(defun make-cond-node (clauses)
  "Create a cond AST node from clauses."
  (make-ast-node :type :cond :value nil :children clauses))

;;; Compile AST to bytecode
(defun compile-to-bytecode (ast &optional (target :rp2040))
  "Compile an AST to SECD bytecode."
  (let ((context (make-compilation-context)))
    (when target
      (load-target-primitives context target))
    (compile-node context ast)
    (emit-opcode context +op-stop+)
    (compilation-context-code context)))

;;; Compile a file
(defun secd-compile-file (filename &key (target :rp2040))
  "Compile a secd-lisp file to SECD bytecode."
  (let* ((source (uiop:read-file-string filename))
         (tokens (tokenize source))
         (ast (parse tokens))
         (bytecode (compile-to-bytecode ast target)))
    bytecode))

;;; Compile a string
(defun compile-string (string &key (target :rp2040))
  "Compile a secd-lisp string to SECD bytecode."
  (let* ((tokens (tokenize string))
         (ast (parse tokens))
         (bytecode (compile-to-bytecode ast target)))
    bytecode))
