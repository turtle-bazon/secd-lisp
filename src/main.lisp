;;;; main.lisp — Main entry point for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Command line interface and main entry points for the compiler.

(in-package #:secd-lisp)

;;; Command line options
(defvar *options* nil
  "Command line options.")

;;; Parse command line arguments
(defun parse-args (args)
  "Parse command line arguments."
  (let ((options nil)
        (input-file nil))
    (loop for arg in args
          do (cond
               ((string= arg "-o")
                (push :output options))
               ((string= arg "-t")
                (push :target options))
               ((string= arg "--lib")
                (push :lib options))
               ((string= arg "--entry")
                (push :entry options))
               ((string= arg "--help")
                (push :help options))
               ((string= arg "--version")
                (push :version options))
               ((and (car options) (eq (car options) :output))
                (pop options)
                (push (cons :output-file arg) options))
               ((and (car options) (eq (car options) :target))
                (pop options)
                (push (cons :target (intern (string-upcase arg) "KEYWORD")) options))
               ((and (car options) (eq (car options) :lib))
                (pop options)
                (push (cons :lib-path arg) options))
               ((and (car options) (eq (car options) :entry))
                (pop options)
                (push (cons :entry-fn arg) options))
               ((null input-file)
                (setf input-file arg))))
    (values input-file (nreverse options))))

;;; Print usage
(defun print-usage ()
  "Print usage information."
  (format t "secd-lisp - secd-lisp compiler for SECD machine~%")
  (format t "~%")
  (format t "Usage: secd-lisp [options] input-file~%")
  (format t "~%")
  (format t "Options:~%")
  (format t "  -o <file>       Output file (default: firmware.secd)~%")
  (format t "  -t <target>     Target platform (rp2040, esp32)~%")
  (format t "  --lib <dir>     Add a library search path~%")
  (format t "  --entry <pkg:fn> Entry point function (default secd:main)~%")
  (format t "  --help          Show this help~%")
  (format t "  --version       Show version~%")
  (format t "~%Library search paths come from the default library/, --lib, and the~%")
  (format t "SECD_LIB environment variable (colon-separated).~%"))

;;; Print version
(defun print-version ()
  "Print version information."
  (format t "secd-lisp v~A~%" (asdf:component-version (asdf:find-system :secd-lisp))))

;;; Main entry point
(defun main (&optional args)
  "Main entry point for secd-lisp compiler."
  (multiple-value-bind (input-file options)
      (parse-args (or args uiop:*command-line-arguments*))
    ;; Handle options
    (when (assoc :help options)
      (print-usage)
      (return-from main 0))
    (when (assoc :version options)
      (print-version)
      (return-from main 0))
    ;; Check input file
    (unless input-file
      (print-usage)
      (return-from main 1))
    ;; Get output file and target
    (let* ((default-output (format nil "~A.uf2"
                                   (pathname-name
                                    (pathname input-file))))
           (output-file (or (cdr (assoc :output-file options))
                            default-output))
           (target-name (or (cdr (assoc :target options))
                            :rp2040))
           (entry-fn (or (cdr (assoc :entry-fn options))
                         "SECD:MAIN")))
      ;; Load machine metadata
      (format t "Loading machine: ~A~%" target-name)
      (set-target target-name)
      (print-target-info)
      ;; Add library search paths from the SECD_LIB environment variable
      (let ((env-lib (uiop:getenv "SECD_LIB")))
        (when (and env-lib (string/= env-lib ""))
          (dolist (dir (uiop:split-string env-lib :separator '(#\; #\:)))
            (when (string/= (string-trim " " dir) "")
              (add-library-search-path (string-trim " " dir))))))
      ;; Add library search paths from --lib
      (let ((lib-option (assoc :lib-path options)))
        (when lib-option
          (add-library-search-path (cdr lib-option))))
      ;; Compile
      (format t "~%Compiling ~A to ~A~%" input-file output-file)
      (handler-case
          (let* ((bytecode (secd-compile-file input-file :target target-name
                                                        :entry entry-fn))
                 (machine-file (target-firmware-path *target*)))
            (format t "Bytecode: ~A bytes~%" (length bytecode))
            (format t "Merging with firmware: ~A~%" machine-file)
            (link-machine machine-file bytecode output-file)
            (format t "Compiled successfully to ~A~%" output-file))
        (error (e)
          (format t "Error: ~A~%" e)
          (return-from main 1))))
    0))
