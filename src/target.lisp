;;;; target.lisp — Load .machine files (compiled secd-machine + metadata)
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Reads .machine files produced by secd-machine project.
;;;; These contain the compiled firmware and metadata about
;;;; available features, primitives, and constraints.

(in-package #:secd-lisp)

;;; .machine file format (zip)
;;; Contains: metadata.json, firmware.uf2

;;; Target metadata structure
(defstruct target
  name
  description
  version
  memory
  flash-layout
  features
  primitives
  constraints
  firmware-path)  ; Path to .machine file

;;; Current target
(defvar *target* nil
  "Current compilation target.")

;;; Search paths for .machine files
(defvar *machine-search-paths*
  (list (asdf:system-relative-pathname :secd-lisp "../secd-machine/output/")
        (merge-pathnames #p".secd-lisp/targets/" (user-homedir-pathname))
        #p"/usr/local/share/secd-lisp/targets/"
        #p"/usr/share/secd-lisp/targets/")
  "Search paths for .machine files.")

;;; Find .machine file
(defun find-machine-file (target-name)
  "Find .machine file for target."
  (let* ((name (if (symbolp target-name) (symbol-name target-name) target-name))
         (filename (format nil "~A.machine" (string-downcase name))))
    (or (loop for path in *machine-search-paths*
              for file = (merge-pathnames filename path)
              when (probe-file file)
                return file)
        ;; Fallback: match <name>-<board>.machine (e.g. rp2040 -> rp2040-pico)
        (let ((candidates
                (loop for path in *machine-search-paths*
                      when (and path (probe-file path))
                        append (directory (merge-pathnames
                                            (format nil "~A-*.machine" (string-downcase name))
                                            path)))))
          (first candidates)))))

;;; Parse section from .machine file
(defun parse-section (stream)
  "Parse a single section from .machine file stream."
  (let ((type (read-byte stream))
        (size (read-byte stream nil 0)))
    ;; Read size as 4 bytes little-endian
    (setf size (+ (read-byte stream)
                  (ash (read-byte stream) 8)
                  (ash (read-byte stream) 16)
                  (ash (read-byte stream) 24)))
    (let ((flags (read-byte stream))
          (data (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      (values type data))))

;;; Load target from .machine file (zip)
(defun load-target (target-name)
  "Load target from .machine file (zip archive)."
  (let ((file (find-machine-file target-name)))
    (unless file
      (error "Machine file not found for target: ~A~%Searched: ~{~A~^, ~}" 
             target-name *machine-search-paths*))
    (zip:with-zipfile (zip file)
      (let* ((entry (zip:get-zipfile-entry "metadata.json" zip))
             (metadata-data (zip:zipfile-entry-contents entry))
             (metadata (yason:parse (map 'string #'code-char metadata-data))))
        (make-target
         :name (gethash "name" metadata)
         :description (gethash "description" metadata)
         :version (gethash "version" metadata)
         :memory (gethash "memory" metadata)
         :flash-layout (gethash "flash_layout" metadata)
         :features (gethash "features" metadata)
         :primitives (gethash "primitives" metadata)
         :constraints (gethash "constraints" metadata)
         :firmware-path file)))))

;;; Set current target
(defun set-target (target-name)
  "Set the current compilation target."
  (setf *target* (load-target target-name))
  *target*)

;;; Build version of secd-lisp (from ASDF)
(defun secd-lisp-version ()
  "Return the secd-lisp build version from the ASDF system definition."
  (asdf:component-version (asdf:find-system :secd-lisp)))

;;; Write target metadata with the CL build version injected
(defun write-target-metadata (target-json output-json)
  "Read TARGET-JSON and write OUTPUT-JSON with its version field set to
the secd-lisp build version (from ASDF)."
  (let ((version (secd-lisp-version)))
    (with-open-file (in target-json :direction :input)
      (let ((metadata (yason:parse in)))
        (setf (gethash "version" metadata) version)
        (with-open-file (out output-json :direction :output
                             :if-exists :supersede)
          (yason:encode metadata out)
          (terpri out))))
    version))

;;; Check if feature is available
(defun has-feature-p (feature)
  "Check if current target has a feature."
  (and *target*
       (let ((features (target-features *target*)))
         (and features
              (find feature features :test #'string=)))))

;;; Get primitive definition
(defun get-primitive (name)
  "Get primitive definition from target."
  (and *target*
       (let ((primitives (target-primitives *target*)))
         (and primitives
              (gethash name primitives)))))

;;; Validate symbol name against constraints
(defun validate-symbol-name (name)
  "Validate symbol name length."
  (when *target*
    (let ((max-len (gethash "max_string_length" (target-constraints *target*))))
      (when (and max-len (> (length name) max-len))
        (error "Symbol name too long: ~A (max: ~A)" (length name) max-len))))
  t)

;;; Validate fixnum range
(defun validate-fixnum (value)
  "Validate fixnum is within target range."
  (when *target*
    (let ((max-val (gethash "max_fixnum" (target-constraints *target*)))
          (min-val (gethash "min_fixnum" (target-constraints *target*))))
      (when (or (and max-val (> value max-val))
                (and min-val (< value min-val)))
        (error "Fixnum out of range: ~A (valid: ~A to ~A)" 
               value min-val max-val))))
  t)

;;; Get flash address for bytecode
(defun bytecode-flash-address ()
  "Get the flash address where bytecode should be placed."
  (and *target*
       (gethash "bytecode_addr" (target-flash-layout *target*))))

;;; Get memory layout info
(defun heap-size ()
  "Get heap size for target."
  (and *target*
       (gethash "heap_size" (target-memory *target*))))

(defun stack-size ()
  "Get stack size for target."
  (and *target*
       (gethash "stack_size" (target-memory *target*))))

;;; Print target info
(defun print-target-info ()
  "Print information about current target."
  (if *target*
      (progn
        (format t "Target: ~A (~A)~%" (target-name *target*) (target-description *target*))
        (format t "Version: ~A~%" (target-version *target*))
        (format t "Features: ~{~A~^, ~}~%" (target-features *target*))
        (format t "Heap: ~A bytes~%" (heap-size))
        (format t "Stack: ~A bytes~%" (stack-size))
        (format t "Bytecode at: 0x~X~%" (bytecode-flash-address)))
      (format t "No target set~%")))
