;;;; codegen.lisp — Code generation for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Generates SECD bytecode from AST.

(in-package #:secd-lisp)

;;; Bytecode header
(defstruct bytecode-header
  "Header for a SECD bytecode file."
  (version 1 :type fixnum)
  (code-size 0 :type fixnum)
  (constants-size 0 :type fixnum)
  (symbols-size 0 :type fixnum)
  (entry-point 0 :type fixnum))

;;; Serialize bytecode header to bytes (14 bytes total, matches C header)
(defun serialize-header (header)
  "Serialize a bytecode header to a byte vector."
  (let ((bytes (make-array 14 :element-type '(unsigned-byte 8))))
    ;; Magic "SECD"
    (setf (aref bytes 0) (char-code #\S))
    (setf (aref bytes 1) (char-code #\E))
    (setf (aref bytes 2) (char-code #\C))
    (setf (aref bytes 3) (char-code #\D))
    ;; Version major/minor (bytes 4-5)
    (setf (aref bytes 4) (logand (bytecode-header-version header) #xff))
    (setf (aref bytes 5) 0)
    ;; Reserved (bytes 6-7)
    (setf (aref bytes 6) 0)
    (setf (aref bytes 7) 0)
    ;; Code size (bytes 8-9)
    (setf (aref bytes 8) (logand (bytecode-header-code-size header) #xff))
    (setf (aref bytes 9) (logand (ash (bytecode-header-code-size header) -8) #xff))
    ;; Constants size (bytes 10-11)
    (setf (aref bytes 10) (logand (bytecode-header-constants-size header) #xff))
    (setf (aref bytes 11) (logand (ash (bytecode-header-constants-size header) -8) #xff))
    ;; Symbols size (bytes 12-13)
    (setf (aref bytes 12) (logand (bytecode-header-symbols-size header) #xff))
    (setf (aref bytes 13) (logand (ash (bytecode-header-symbols-size header) -8) #xff))
    bytes))

;;; Generate bytecode file
(defun generate-bytecode-file (code constants output-file)
  "Generate a complete bytecode file with header."
  (let* ((code-vec (if (vectorp code) code (coerce code 'vector)))
         (header (make-bytecode-header :code-size (length code-vec)
                                       :constants-size (length constants)
                                       :symbols-size 0)))
    (with-open-file (stream output-file :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede)
      ;; Write header
      (write-sequence (serialize-header header) stream)
      ;; Write code
      (write-sequence code-vec stream)
      ;; Write constants (TODO: implement constant serialization)
      ;; Write symbols (TODO: implement symbol serialization)
      ))
  output-file)

;;; Write bytecode to file
(defun write-bytecode (bytecode output-file)
  "Write bytecode to a file."
  (with-open-file (stream output-file :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (write-sequence bytecode stream))
  output-file)
