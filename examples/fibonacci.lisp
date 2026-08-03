;;;; fibonacci.lisp — Fibonacci example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Computes fibonacci numbers.

;; Fibonacci function
(defun fibonacci (n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Print fibonacci of 10
(print (fibonacci 10))
