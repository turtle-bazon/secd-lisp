;;;; parser-tests.lisp — Tests for parser
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(in-package #:secd-lisp/test)

(def-suite parser-tests
  :description "Tests for the parser")

(in-suite parser-tests)

(test parse-symbol
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "foo"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-value ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :symbol))
      (is (eq (secd-lisp:ast-node-value expr) (secd-foo))))))

(test parse-integer
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "42"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-value ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :integer))
      (is (= (secd-lisp:ast-node-value expr) 42)))))

(test parse-list
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "(+ 1 2)"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-value ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :application)))))

(test parse-quote
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "'foo"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-value ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :quote)))))

(test parse-byte-vector
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#(1 2 42 4)"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-value ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :byte-vector))
      (is (equal (secd-lisp:ast-node-value expr) '(1 2 42 4))))))

(test parse-setf-vref-place
  ;; (setf (vref vec idx) val) must keep the application as the place
  ;; so the compiler can see vec and idx.
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "(setf (vref v 1) 200)"))))
    (let ((expr (first (secd-lisp:ast-node-value ast))))
      (is (eq (secd-lisp:ast-node-type expr) :setf))
      (is (secd-lisp:ast-application-p (secd-lisp:ast-node-value expr)))
      (is (string= (symbol-name (secd-lisp:ast-node-value
                                  (secd-lisp:ast-node-value
                                    (secd-lisp:ast-node-value expr))))
                   "VREF")))))
