;;;; factorial.lisp — Factorial example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Computes factorial using conditional.

;; Simple factorial using if
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Entry point: print factorial of 5
(defun main ()
  (print (factorial 5)))
