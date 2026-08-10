;;;; package.lisp — Package definitions for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Defines the packages for the secd-lisp compiler.

(defpackage #:secd-lisp
  (:use #:cl #:iterate #:metabang-bind #:alexandria)
  (:export
   ;; Lexer
   #:token
   #:token-type
   #:token-value
   #:token-line
   #:token-column
   #:make-token
   #:tokenize
   
   ;; Parser
   #:parse
   #:parse-expression
   #:ast-node
   #:ast-node-type
   #:ast-node-value
   #:ast-node-children
   #:make-ast-node
   #:make-program
   #:make-symbol-node
   #:make-integer-node
   #:make-string-node
   #:make-boolean-node
   #:make-nil-node
   #:make-if-node
   #:make-lambda-node
   #:make-defun-node
   #:make-defvar-node
   #:make-application-node
   #:make-quote-node
   #:make-progn-node
   #:make-setf-node
   #:make-byte-vector-node
   #:ast-symbol-p
   #:ast-application-p
   #:ast-byte-vector-p
   #:ast-literal-p
   
   ;; Compiler
   #:secd-compile-file
   #:compile-string
   #:compile-to-bytecode
   #:compilation-context
   #:make-compilation-context
   
   ;; Code generation
   #:generate-bytecode-file
   #:write-bytecode
   #:bytecode-header
   #:make-bytecode-header
   
    ;; Linker
    #:link
    #:link-machine
    #:load-library
    #:save-library
    #:library
    #:make-library
   
   ;; Tree shaker
   #:shake-tree
   #:build-call-graph
   #:find-reachable
   
    ;; Main
    #:main
    #:print-usage
    #:print-version
    
    ;; Target
    #:set-target
    #:print-target-info
    #:has-feature-p
    #:get-primitive
    #:bytecode-flash-address
    #:secd-lisp-version
    #:write-target-metadata))
