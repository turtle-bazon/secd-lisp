;;;; lists.lisp — List operations example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Demonstrates list operations.

;; Create a list
(defun make-list ()
  (cons 1 (cons 2 (cons 3 nil))))

;; Get car and cdr
(defun main ()
  (print (make-list))
  (print (car (cons 1 2)))
  (print (cdr (cons 1 2)))
  ;; Predicates: null?, pair?, atom?
  (print (null? nil))
  (print (null? (cons 1 nil)))
  (print (pair? (cons 1 2)))
  (print (atom? 5)))
