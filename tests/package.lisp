;;;; package.lisp — Test package definitions for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(defpackage #:secd-lisp/test
  (:use #:cl #:fiveam #:secd-lisp)
  (:export
   #:run-tests))
