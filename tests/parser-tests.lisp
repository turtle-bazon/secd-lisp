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
    (is (= (length (secd-lisp:ast-node-children ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-children ast))))
      (is (eq (secd-lisp:ast-node-type expr) :symbol))
      (is (eq (secd-lisp:ast-node-value expr) 'foo)))))

(test parse-integer
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "42"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-children ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-children ast))))
      (is (eq (secd-lisp:ast-node-type expr) :integer))
      (is (= (secd-lisp:ast-node-value expr) 42)))))

(test parse-list
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "(+ 1 2)"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-children ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-children ast))))
      (is (eq (secd-lisp:ast-node-type expr) :application)))))

(test parse-quote
  (let ((ast (secd-lisp:parse (secd-lisp:tokenize "'foo"))))
    (is (eq (secd-lisp:ast-node-type ast) :program))
    (is (= (length (secd-lisp:ast-node-children ast)) 1))
    (let ((expr (first (secd-lisp:ast-node-children ast))))
      (is (eq (secd-lisp:ast-node-type expr) :quote)))))
