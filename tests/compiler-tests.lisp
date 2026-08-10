;;;; compiler-tests.lisp — Tests for compiler
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(in-package #:secd-lisp/test)

(def-suite compiler-tests
  :description "Tests for the compiler")

(in-suite compiler-tests)

(defun bytecode-has (bytecode opcode)
  (find opcode bytecode))

(test compile-integer
  (let ((bytecode (secd-lisp:compile-string "(defun main () 42)")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))
    ;; Layout is [JMP entry][defun blobs][call secd:main]; a program
    ;; always contains an LDC instruction for the literal.
    (is (bytecode-has bytecode #x02))))

(test compile-symbol
  (let ((bytecode (secd-lisp:compile-string "(defun main () t)")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))
    (is (bytecode-has bytecode #x02))))

(test compile-add
  (let ((bytecode (secd-lisp:compile-string "(defun main () (+ 1 2))")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))
    ;; ADD primitive opcode
    (is (bytecode-has bytecode #x10))))

(test compile-requires-main
  ;; Entry point is always (defun main ()), so a bare top-level
  ;; expression is rejected.
  (signals error
    (secd-lisp:compile-string "42")))

(test compile-byte-vector-literal
  ;; #(...) literal must emit LDV (0x33) plus the ROM pool footer
  ;; (magic 0xB1C5 at the end) so the C loader can find the bytes.
  (let ((bytecode (secd-lisp:compile-string "(defun main () #(1 2 42 4))")))
    (is (bytecode-has bytecode #x33))                 ; LDV
    (is (bytecode-has bytecode #x2A))                 ; pool data 42
    (is (bytecode-has bytecode #xB1))                 ; pool magic hi
    (is (bytecode-has bytecode #xC5))))               ; pool magic lo

(test compile-byte-vector-vref-mass
  ;; vref/length/make-vector are builtin opcodes: VREF 0x34, LEN 0x37, MKV 0x36
  (let ((bytecode (secd-lisp:compile-string
                     "(defun main () (length #(1 2 3)))")))
      (is (bytecode-has bytecode #x37))))                ; LEN

(test compile-byte-vector-setf-vref
  ;; (setf (vref vec i) b) compiles to VSTOR (0x35); vec built via
  ;; make-vector -> MKV (0x36).
  (let ((bytecode (secd-lisp:compile-string
                   "(defun main () (let ((v (make-vector 4))) (setf (vref v 1) 200)))")))
    (is (bytecode-has bytecode #x36))                 ; MKV
    (is (bytecode-has bytecode #x35))))               ; VSTOR

(test compile-byte-vector-let-bindings
  ;; LET with byte-vector init and vref access compiles (regression for the
  ;; pre-existing let binding-list bug fixed alongside byte-vector support).
  (let ((bytecode (secd-lisp:compile-string
                   "(defun main () (let ((v #(1 2 3))) (vref v 1)))")))
    (is (bytecode-has bytecode #x33))                 ; LDV
    (is (bytecode-has bytecode #x34))))               ; VREF
