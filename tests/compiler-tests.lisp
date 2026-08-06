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
