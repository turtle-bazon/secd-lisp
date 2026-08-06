;;;; conditionals.lisp — Conditionals example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Demonstrates conditional expressions. Entry point is (defun main ()).

;; Simple if
(defun abs (x)
  (if (< x 0)
      (neg x)
      x))

;; When
(defun is-positive (x)
  (when (> x 0)
    t))

;; Unless
(defun is-non-positive (x)
  (unless (> x 0)
    t))

(defun main ()
  (print (abs -5))
  (print (abs 5))
  (print (is-positive 5))
  (print (is-positive -5))
  (print (is-non-positive -5))
  (print (is-non-positive 5)))