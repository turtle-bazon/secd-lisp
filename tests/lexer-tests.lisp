;;;; lexer-tests.lisp — Tests for lexer
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(in-package #:secd-lisp/test)

(def-suite lexer-tests
  :description "Tests for the lexer")

(in-suite lexer-tests)

(defun secd-foo () (find-symbol "FOO" "SECD-LISP"))

(test tokenize-symbol
  (let ((tokens (secd-lisp:tokenize "foo")))
    (is (= (length tokens) 2)) ; symbol + eof
    (is (eq (secd-lisp:token-type (first tokens)) :symbol))
    (is (eq (secd-lisp:token-value (first tokens)) (secd-foo)))))

(test tokenize-integer
  (let ((tokens (secd-lisp:tokenize "42")))
    (is (= (length tokens) 2)) ; integer + eof
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) 42))))

(test tokenize-hex-integer
  (let ((tokens (secd-lisp:tokenize "0xFF")))
    (is (= (length tokens) 2)) ; integer + eof
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) 255))))

(test tokenize-hex-integer-lowercase
  (let ((tokens (secd-lisp:tokenize "0x5e")))
    (is (= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) 94))))

(test tokenize-binary-integer
  (let ((tokens (secd-lisp:tokenize "0b1010")))
    (is (= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) 10))))

(test tokenize-negative-hex-integer
  (let ((tokens (secd-lisp:tokenize "-0x1F")))
    (is (= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :integer))
    (is (= (secd-lisp:token-value (first tokens)) -31))))

(test tokenize-hex-in-byte-vector
  (let ((tokens (secd-lisp:tokenize "#(0x00 0x5E 0xFF 0b0001)")))
    (is (= (length tokens) 2)) ; byte-vector + eof
    (is (eq (secd-lisp:token-type (first tokens)) :byte-vector))
    (is (equal (secd-lisp:token-value (first tokens)) '(0 94 255 1)))))

(test tokenize-hex-byte-vector-out-of-range
  (signals error
    (secd-lisp:tokenize "#(0x100 0x00)")))

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
    (is (= (length tokens) 6)))) ; ( + 1 2 ) + eof

(test tokenize-quote
  (let ((tokens (secd-lisp:tokenize "'foo")))
    (is (= (length tokens) 3)) ; ' foo + eof
    (is (eq (secd-lisp:token-type (first tokens)) :quote))))

(test tokenize-string-with-escape
  (let ((tokens (secd-lisp:tokenize "\"hello\\nworld\"")))
    (is (= (length tokens) 2)) ; string + eof
    (is (eq (secd-lisp:token-type (first tokens)) :string))
    ;; NOTE: `\n` in this test is backslash + n; the lexer maps it to newline.
    (is (string= (secd-lisp:token-value (first tokens))
                 (concatenate 'string "hello" (string #\Newline) "world")))))

(test tokenize-byte-vector
  (let ((tokens (secd-lisp:tokenize "#(1 2 42 4)")))
    (is (= (length tokens) 2)) ; byte-vector + eof
    (is (eq (secd-lisp:token-type (first tokens)) :byte-vector))
    (is (equal (secd-lisp:token-value (first tokens)) '(1 2 42 4)))))

(test tokenize-byte-vector-empty
  (let ((tokens (secd-lisp:tokenize "#()")))
    (is (= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :byte-vector))
    (is (equal (secd-lisp:token-value (first tokens)) nil))))

(test tokenize-byte-vector-out-of-range
  (signals error
    (secd-lisp:tokenize "#(0 256)")))

(test tokenize-byte-vector-non-integer
  (signals error
    (secd-lisp:tokenize "#(a b)")))

(test tokenize-sharp-cond-plus
  (let ((tokens (secd-lisp:tokenize "#+esp32 1")))
    (is (>= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :sharp-cond))
    (is (eq (secd-lisp:token-value (first tokens)) :plus))
    (is (eq (secd-lisp:token-type (second tokens)) :symbol))
    (is (string-equal (string (secd-lisp:token-value (second tokens))) "esp32"))))

(test tokenize-sharp-cond-minus
  (let ((tokens (secd-lisp:tokenize "#-esp32 1")))
    (is (>= (length tokens) 2))
    (is (eq (secd-lisp:token-type (first tokens)) :sharp-cond))
    (is (eq (secd-lisp:token-value (first tokens)) :minus))))
