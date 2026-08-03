;;;; lists.lisp — List operations example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Demonstrates list operations.

;; Create a list
(print (cons 1 (cons 2 (cons 3 nil))))

;; Get car and cdr
(print (car (cons 1 2)))
(print (cdr (cons 1 2)))

;; Conditional with lists
(defun null-check (lst)
  (if (null lst)
      t
      nil))

(print (null-check nil))
(print (null-check (cons 1 nil)))
