;;;; ast.lisp — Abstract Syntax Tree for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Defines the AST node types for the secd-lisp compiler.

(in-package #:secd-lisp)

;;; AST node types
(defstruct (ast-node (:constructor make-ast-node
                      (&key (type :unknown) value (children nil) (line 0) (column 0))))
  "Represents a node in the abstract syntax tree."
  (type nil :type keyword)
  (value nil)
  (children nil :type list)
  (line 0 :type fixnum)
  (column 0 :type fixnum))

;;; AST node types
;; :program       - Top-level program (list of forms)
;; :symbol        - Symbol reference
;; :integer       - Integer literal
;; :float         - Float literal
;; :string        - String literal
;; :character     - Character literal
;; :boolean       - Boolean literal
;; :nil           - Nil literal
;; :list          - List literal
;; :pair          - Cons pair
;; :if            - Conditional
;; :when          - When conditional
;; :unless        - Unless conditional
;; :cond          - Cond expression
;; :let           - Let binding
;; :let*          - Let* binding
;; :lambda        - Lambda expression
;; :defun         - Function definition
;; :defvar        - Variable definition
;; :defmacro      - Macro definition
;; :progn         - Sequence of expressions
;; :quote         - Quote expression
;; :quasiquote    - Quasiquote expression
;; :unquote       - Unquote expression
;; :splice        - Splice expression
;; :application   - Function application
;; :setf          - Set! expression
;; :block         - Block expression
;; :return-from   - Return from block
;; :tagbody       - Tagbody expression
;; :go            - Go to tag
;; :loop          - Loop expression
;; :dotimes       - Dotimes iteration
;; :dolist        - Dolist iteration
;; :values        - Multiple values
;; :multiple-value-bind - Bind multiple values

;;; Helper functions

(defun make-program (forms &key (line 0) (column 0))
  "Create a program node from a list of forms."
  (make-ast-node :type :program :value forms :line line :column column))

(defun make-symbol-node (symbol &key (line 0) (column 0))
  "Create a symbol reference node."
  (make-ast-node :type :symbol :value symbol :line line :column column))

(defun make-integer-node (value &key (line 0) (column 0))
  "Create an integer literal node."
  (make-ast-node :type :integer :value value :line line :column column))

(defun make-string-node (value &key (line 0) (column 0))
  "Create a string literal node."
  (make-ast-node :type :string :value value :line line :column column))

(defun make-boolean-node (value &key (line 0) (column 0))
  "Create a boolean literal node."
  (make-ast-node :type :boolean :value value :line line :column column))

(defun make-nil-node (&key (line 0) (column 0))
  "Create a nil literal node."
  (make-ast-node :type :nil :value nil :line line :column column))

(defun make-if-node (condition then else &key (line 0) (column 0))
  "Create an if expression node."
  (make-ast-node :type :if :value nil
                 :children (list condition then else)
                 :line line :column column))

(defun make-lambda-node (params body &key (line 0) (column 0))
  "Create a lambda expression node."
  (make-ast-node :type :lambda :value params
                 :children (if (listp body) body (list body))
                 :line line :column column))

(defun make-defun-node (name params body &key (line 0) (column 0))
  "Create a function definition node."
  (make-ast-node :type :defun :value (list name params)
                 :children (if (listp body) body (list body))
                 :line line :column column))

(defun make-defvar-node (name value &key (line 0) (column 0))
  "Create a variable definition node."
  (make-ast-node :type :defvar :value name
                 :children (list value)
                 :line line :column column))

(defun make-defconstant-node (name value &key (line 0) (column 0))
  "Create a constant definition node."
  (make-ast-node :type :defconstant :value name
                 :children (list value)
                 :line line :column column))

(defun make-application-node (operator operands &key (line 0) (column 0))
  "Create a function application node."
  (make-ast-node :type :application :value operator
                 :children operands
                 :line line :column column))

(defun make-quote-node (value &key (line 0) (column 0))
  "Create a quote expression node."
  (make-ast-node :type :quote :value value :line line :column column))

(defun make-progn-node (body &key (line 0) (column 0))
  "Create a progn expression node."
  (make-ast-node :type :progn :value nil
                 :children body
                 :line line :column column))

(defun make-setf-node (name value &key (line 0) (column 0))
  "Create a set! expression node."
  (make-ast-node :type :setf :value name
                 :children (list value)
                 :line line :column column))

;;; Helper predicates

(defun ast-program-p (node)
  "Check if node is a program node."
  (eq (ast-node-type node) :program))

(defun ast-symbol-p (node)
  "Check if node is a symbol reference."
  (eq (ast-node-type node) :symbol))

(defun ast-integer-p (node)
  "Check if node is an integer literal."
  (eq (ast-node-type node) :integer))

(defun ast-literal-p (node)
  "Check if node is a literal (not a symbol or application)."
  (member (ast-node-type node) '(:integer :float :string :character :boolean :nil :list)))

(defun ast-application-p (node)
  "Check if node is a function application."
  (eq (ast-node-type node) :application))
