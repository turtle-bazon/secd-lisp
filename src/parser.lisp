;;;; parser.lisp — Parser for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Converts a list of tokens into an abstract syntax tree.

(in-package #:secd-lisp)

;;; Parser state
(defstruct (parser (:constructor %make-parser))
  "State of the parser during parsing."
  (tokens nil :type list)
  (position 0 :type fixnum))

;;; Create a new parser
(defun make-parser (tokens)
  "Create a new parser for the given token list."
  (%make-parser :tokens tokens :position 0))

;;; Peek at current token
(defun parser-peek (parser &optional (offset 0))
  "Peek at the token at current position + offset."
  (let ((pos (+ (parser-position parser) offset)))
    (when (< pos (length (parser-tokens parser)))
      (nth pos (parser-tokens parser)))))

;;; Advance position
(defun parser-advance (parser)
  "Advance the parser position by one token."
  (let ((token (parser-peek parser)))
    (incf (parser-position parser))
    token))

;;; Expect a specific token type
(defun parser-expect (parser type)
  "Expect and consume a token of the given type."
  (let ((token (parser-peek parser)))
    (unless token
      (error "Unexpected end of input, expected ~A" type))
    (unless (eq (token-type token) type)
      (error "Expected ~A at line ~A, column ~A, got ~A"
             type (token-line token) (token-column token) (token-type token)))
    (parser-advance parser)))

;;; Parse a single expression
(defun parse-expression (parser)
  "Parse a single expression from the token stream."
  (let ((token (parser-peek parser)))
    (unless token
      (error "Unexpected end of input"))
    (case (token-type token)
      ;; Integer literal
      (:integer
       (parser-advance parser)
       (make-integer-node (token-value token)
                          :line (token-line token)
                          :column (token-column token)))
      
      ;; Float literal
      (:float
       (parser-advance parser)
       (make-ast-node :type :float :value (token-value token)
                      :line (token-line token) :column (token-column token)))
      
      ;; String literal
      (:string
       (parser-advance parser)
       (make-string-node (token-value token)
                         :line (token-line token)
                         :column (token-column token)))
      
      ;; Character literal
      (:character
       (parser-advance parser)
       (make-ast-node :type :character :value (token-value token)
                      :line (token-line token) :column (token-column token)))
      
      ;; Boolean literal
      (:boolean
       (parser-advance parser)
       (make-boolean-node (token-value token)
                          :line (token-line token)
                          :column (token-column token)))
      
      ;; Nil
      (:nil
       (parser-advance parser)
       (make-nil-node :line (token-line token) :column (token-column token)))
      
      ;; Symbol
      (:symbol
       (parser-advance parser)
       (make-symbol-node (token-value token)
                         :line (token-line token)
                         :column (token-column token)))
      
      ;; Keyword
      (:keyword
       (parser-advance parser)
       (make-symbol-node (token-value token)
                         :line (token-line token)
                         :column (token-column token)))
      
      ;; Left paren - list or application
      (:left-paren
       (parse-list parser))
      
      ;; Quote
      (:quote
       (parser-advance parser)
       (let ((value (parse-expression parser)))
         (make-quote-node value
                          :line (token-line token)
                          :column (token-column token))))
      
      ;; Quasiquote
      (:quasiquote
       (parser-advance parser)
       (let ((value (parse-expression parser)))
         (make-ast-node :type :quasiquote :value value
                        :line (token-line token) :column (token-column token))))
      
      ;; Unquote
      (:unquote
       (parser-advance parser)
       (let ((value (parse-expression parser)))
         (make-ast-node :type :unquote :value value
                        :line (token-line token) :column (token-column token))))
      
      ;; Splice
      (:splice
       (parser-advance parser)
       (let ((value (parse-expression parser)))
         (make-ast-node :type :splice :value value
                        :line (token-line token) :column (token-column token))))
      
      ;; Dot
      (:dot
       (error "Unexpected dot at line ~A, column ~A"
              (token-line token) (token-column token)))
      
      ;; Right paren
      (:right-paren
       (error "Unexpected right parenthesis at line ~A, column ~A"
              (token-line token) (token-column token)))
      
      ;; EOF
      (:eof
       (error "Unexpected end of input"))
      
      (t
       (error "Unexpected token type ~A at line ~A, column ~A"
              (token-type token) (token-line token) (token-column token))))))

;;; Special form names (compared by symbol-name string)
(defparameter *special-form-names*
  '("DEFUN" "LAMBDA" "IF" "WHEN" "UNLESS" "COND" "LET" "LET*"
    "PROGN" "SETF" "SET!" "LOOP" "DEFVAR" "DEFCONSTANT" "QUOTE"))

(defun special-form-name-p (name)
  "Check if a symbol name is a special form."
  (and (symbolp name)
       (member (symbol-name name) *special-form-names*
               :test #'string=)))

;;; Convert a parsed parameter-list node back to a list of symbols.
;;; `()` -> nil, `(x)` -> (x), `(x y)` -> (x y).
(defun parse-param-list (node)
  "Extract a list of parameter symbols from a parsed list node."
  (cond
    ((null node) nil)
    ((ast-application-p node)
     (let ((op (ast-node-value node)))
       (cond
         ((and (ast-symbol-p op)
               (string= (symbol-name (ast-node-value op)) "FUNCALL")
               (null (ast-node-children node)))
          nil)  ; ()
         ((ast-symbol-p op)
          (cons (ast-node-value op)
                (mapcar #'ast-node-value (ast-node-children node))))
         (t nil))))
    (t nil)))

;;; Build an AST node from a parsed list's elements.
;;; Elements are collected in reverse order (via push).
(defun build-list-node (elements open-token)
  "Build the AST node for a list from its (reversed) elements."
  (let ((forward (nreverse elements))
        (line (token-line open-token))
        (col (token-column open-token)))
    (cond
      ;; Empty list ()
      ((null forward)
       (make-application-node
        (make-symbol-node 'secd-lisp::funcall :line line :column col)
        nil :line line :column col))
      ;; Special form
      ((and (ast-symbol-p (car forward))
            (special-form-name-p (ast-node-value (car forward))))
       (make-special-form-node (ast-node-value (car forward))
                               (cdr forward) open-token))
      ;; Regular application: operator is the FIRST element
      (t
       (make-application-node (car forward) (cdr forward)
                              :line line :column col)))))

(defun make-special-form-node (name args open-token)
  "Build an AST node for a special form."
  (let ((line (token-line open-token))
        (col (token-column open-token))
        (sname (symbol-name name)))
    (cond
      ((string= sname "DEFUN")
       (make-defun-node (ast-node-value (first args))
                        (parse-param-list (second args))
                        (cddr args) :line line :column col))
      ((string= sname "LAMBDA")
       (make-lambda-node (parse-param-list (first args))
                         (rest args) :line line :column col))
      ((string= sname "IF")
       (when (< (length args) 3)
         (error "if requires at least 3 arguments"))
       (make-if-node (first args) (second args) (third args)
                     :line line :column col))
      ((string= sname "WHEN")
       (make-ast-node :type :when :children args
                      :line line :column col))
      ((string= sname "UNLESS")
       (make-ast-node :type :unless :children args
                      :line line :column col))
      ((string= sname "COND")
       (make-ast-node :type :cond :children args
                      :line line :column col))
      ((string= sname "LET")
       (make-ast-node :type :let :children args
                      :line line :column col))
      ((string= sname "LET*")
       (make-ast-node :type :let* :children args
                      :line line :column col))
      ((string= sname "PROGN")
       (make-progn-node args :line line :column col))
      ((or (string= sname "SETF") (string= sname "SET!"))
       (make-setf-node (ast-node-value (first args)) (second args)
                       :line line :column col))
      ((string= sname "LOOP")
       (make-ast-node :type :loop :children args
                      :line line :column col))
      ((string= sname "DEFVAR")
       (make-defvar-node (ast-node-value (first args)) (second args)
                         :line line :column col))
      ((string= sname "DEFCONSTANT")
       (make-defconstant-node (ast-node-value (first args)) (second args)
                              :line line :column col))
      ((string= sname "QUOTE")
       (make-quote-node (first args) :line line :column col))
      (t
       (error "Unknown special form: ~A" name)))))

;;; Parse a list
(defun parse-list (parser)
  "Parse a list from the token stream."
  (let ((open-token (parser-expect parser :left-paren))
        (elements nil)
        (dotted nil))
    (loop
      (let ((token (parser-peek parser)))
        (cond
          ;; End of list
          ((null token)
           (error "Unexpected end of input in list"))
          ((eq (token-type token) :right-paren)
           (parser-advance parser)
           (return (if dotted
                       (make-ast-node :type :pair :value (nreverse elements)
                                      :line (token-line open-token)
                                      :column (token-column open-token))
                       (build-list-node elements open-token))))
          ;; Dotted pair
          ((eq (token-type token) :dot)
           (parser-advance parser)
           (setf dotted (parse-expression parser)))
          ;; Regular element
          (t
           (push (parse-expression parser) elements)))))))

;;; Parse a list of expressions
(defun parse-expressions (parser)
  "Parse a list of expressions until EOF."
  (let ((expressions nil))
    (loop
      (let ((token (parser-peek parser)))
        (when (or (null token) (eq (token-type token) :eof))
          (return (nreverse expressions)))
        (push (parse-expression parser) expressions)))))

;;; Parse tokens into AST
(defun parse (tokens)
  "Parse a list of tokens into an AST."
  (let ((parser (make-parser tokens)))
    (let ((expressions (parse-expressions parser)))
      (make-program expressions))))
