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
  (format t "  --lib <dir>     Library search path~%")
  (format t "  --help          Show this help~%")
  (format t "  --version       Show version~%"))

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
                            :rp2040)))
      ;; Load machine metadata
      (format t "Loading machine: ~A~%" target-name)
      (set-target target-name)
      (print-target-info)
      ;; Compile
      (format t "~%Compiling ~A to ~A~%" input-file output-file)
      (handler-case
          (let* ((bytecode (secd-compile-file input-file :target target-name))
                 (machine-file (target-firmware-path *target*)))
            (format t "Bytecode: ~A bytes~%" (length bytecode))
            (format t "Merging with firmware: ~A~%" machine-file)
            (link-machine machine-file bytecode output-file)
            (format t "Compiled successfully to ~A~%" output-file))
        (error (e)
          (format t "Error: ~A~%" e)
          (return-from main 1))))
    0))
