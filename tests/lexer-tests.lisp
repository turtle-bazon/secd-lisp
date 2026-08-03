;;;; lexer-tests.lisp — Tests for lexer
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(in-package #:secd-lisp/test)

(def-suite lexer-tests
  :description "Tests for the lexer")

(in-suite lexer-tests)

(test tokenize-symbol
  (let ((tokens (secd-lisp:tokenize "foo")))
    (is (= (length tokens) 2)) ; symbol + eof
    (is (eq (secd-lisp:token-type (first tokens)) :symbol))
    (is (eq (secd-lisp:token-value (first tokens)) 'foo))))

(test tokenize-integer
  (let ((tokens (secd-lisp:tokenize "42")))
    (is (= (length tokens) 2)) ; integer + eof
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) 42))))

(test tokenize-string
  (let ((tokens (secd-lisp:tokenize "\"hello\"")))
    (is (= (length tokens) 2)) ; string + eof
    (is (eq (secd-lisp:token-type (first tokens)) :string))
    (is (string= (secd-lisp:token-value (first tokens)) "hello"))))

(test tokenize-boolean
  (let ((tokens (secd-lisp:tokenize "#t")))
    (is (= (length tokens) 2)) ; boolean + eof
    (is (eq (secd-lisp:token-type (first tokens)) :boolean))
    (is (eq (secd-lisp:token-value (first tokens)) t))))

(test tokenize-list
  (let ((tokens (secd-lisp:tokenize "(+ 1 2)")))
    (is (= (length tokens) 7)))) ; ( + 1 2 ) + eof

(test tokenize-quote
  (let ((tokens (secd-lisp:tokenize "'foo")))
    (is (= (length tokens) 3)) ; ' foo + eof
    (is (eq (secd-lisp:token-type (first tokens)) :quote))))

(test tokenize-string-with-escape
  (let ((tokens (secd-lisp:tokenize "\"hello\\nworld\"")))
    (is (= (length tokens) 2)) ; string + eof
    (is (eq (secd-lisp:token-type (first tokens)) :string))
    (is (string= (secd-lisp:token-value (first tokens)) "hello\nworld"))))
