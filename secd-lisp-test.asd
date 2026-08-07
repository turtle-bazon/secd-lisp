;;;; secd-lisp-test.asd — Test system definition for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(asdf:defsystem #:secd-lisp-test
  :description "Tests for secd-lisp compiler"
  :author "Your Name"
  :license "GPL3"
  :version "0.0.1.0"
  :depends-on (#:secd-lisp
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "lexer-tests")
                             (:file "parser-tests")
                             (:file "compiler-tests")))))