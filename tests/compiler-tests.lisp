;;;; compiler-tests.lisp — Tests for compiler
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(in-package #:secd-lisp/test)

(def-suite compiler-tests
  :description "Tests for the compiler")

(in-suite compiler-tests)

(test compile-integer
  (let ((bytecode (secd-lisp:compile-string "42")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))
    ;; Should contain LDC instruction
    (is (= (aref bytecode 0) #x02))))

(test compile-symbol
  (let ((bytecode (secd-lisp:compile-string "t")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))))

(test compile-add
  (let ((bytecode (secd-lisp:compile-string "(+ 1 2)")))
    (is (not (null bytecode)))
    (is (vectorp bytecode))))
