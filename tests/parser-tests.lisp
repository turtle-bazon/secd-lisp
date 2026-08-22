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

(test parse-sharp-cond-include
  (let ((secd-lisp:*compile-features* (list "ESP32")))
    (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+esp32 42"))))
      (is (= (length (secd-lisp:ast-node-value ast)) 1))
      (let ((expr (first (secd-lisp:ast-node-value ast))))
        (is (eq (secd-lisp:ast-node-type expr) :integer))
        (is (= (secd-lisp:ast-node-value expr) 42))))))

(test parse-sharp-cond-exclude
  (let ((secd-lisp:*compile-features* nil))
    (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+esp32 42"))))
      ;; Skipped top-level forms vanish entirely.
      (is (= (length (secd-lisp:ast-node-value ast)) 0)))))


(test parse-sharp-cond-minus-includes-when-absent
  (let ((secd-lisp:*compile-features* nil))
    (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#-esp32 7"))))
      (is (eq (secd-lisp:ast-node-type
               (first (secd-lisp:ast-node-value ast))) :integer))
      (is (= (secd-lisp:ast-node-value
              (first (secd-lisp:ast-node-value ast))) 7)))))

(test parse-sharp-cond-or-spec
  (progn
    (let ((secd-lisp:*compile-features* (list "RP2040")))
      (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+(or rp2040 rp2350) 1"))))
        (is (eq (secd-lisp:ast-node-type
                 (first (secd-lisp:ast-node-value ast))) :integer))))
    (let ((secd-lisp:*compile-features* nil))
      (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+(or rp2040 rp2350) 1"))))
        (is (= (length (secd-lisp:ast-node-value ast)) 0))))))

(test parse-sharp-cond-not-spec
  (progn
    (let ((secd-lisp:*compile-features* (list "RP2040")))
      (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+(not esp32) 1"))))
        (is (eq (secd-lisp:ast-node-type
                 (first (secd-lisp:ast-node-value ast))) :integer))))
    (let ((secd-lisp:*compile-features* (list "ESP32")))
      (let ((ast (secd-lisp:parse (secd-lisp:tokenize "#+(not esp32) 1"))))
        (is (= (length (secd-lisp:ast-node-value ast)) 0))))))

(test parse-sharp-cond-skips-vanish-inside-lists
  (let ((secd-lisp:*compile-features* nil))
    (let ((ast (secd-lisp:parse (secd-lisp:tokenize "(list 1 #+nope 2 3)"))))
      (let ((call (first (secd-lisp:ast-node-value ast))))
        ;; skipped operand vanishes: operands are 1 and 3
        (is (= (length (secd-lisp:ast-node-children call)) 2))))))

