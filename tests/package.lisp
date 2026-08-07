;;;; package.lisp — Test package definitions for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(defpackage #:secd-lisp/test
  (:use #:cl #:fiveam #:secd-lisp)
  (:export
   #:run-tests))

(in-package #:secd-lisp/test)

(defun run-tests ()
  "Run the full FiveAM suite; return T when everything passes."
  (let ((results (list (fiveam:run! 'lexer-tests)
                       (fiveam:run! 'parser-tests)
                       (fiveam:run! 'compiler-tests))))
    (every #'identity results)))
