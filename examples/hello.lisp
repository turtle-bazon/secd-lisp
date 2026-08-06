;;;; hello.lisp — Hello World example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Simple hello world program. The entry point is (defun main ()).

;; Print a number
(defun main ()
  (print 42)
  ;; Print result of arithmetic
  (print (+ 1 2)))