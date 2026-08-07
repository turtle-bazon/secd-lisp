;;;; secd-lisp.asd — System definition for secd-lisp compiler
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3

(asdf:defsystem #:secd-lisp
  :description "secd-lisp compiler for SECD machine"
  :author "Your Name"
  :license "GPL3"
  :version "0.0.1.0"
  :depends-on (#:iterate
               #:metabang-bind
               #:cl-bazon
               #:cl-ppcre
               #:alexandria
               #:yason
               #:zip
               #:clingon)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lexer")
                             (:file "parser")
                             (:file "ast")
                             (:file "target")
                             (:file "compiler")
                             (:file "codegen")
                             (:file "linker")
                             (:file "tree-shaker")
                             (:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/secd-lisp"
  :entry-point "secd-lisp:main")
