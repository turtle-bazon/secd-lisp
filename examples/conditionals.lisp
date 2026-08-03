;;;; conditionals.lisp — Conditionals example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Demonstrates conditional expressions.

;; Simple if
(defun abs (x)
  (if (< x 0)
      (neg x)
      x))

(print (abs -5))
(print (abs 5))

;; When
(defun is-positive (x)
  (when (> x 0)
    t))

(print (is-positive 5))
(print (is-positive -5))

;; Unless
(defun is-non-positive (x)
  (unless (> x 0)
    t))

(print (is-non-positive -5))
(print (is-non-positive 5))
