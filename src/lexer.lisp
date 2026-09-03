;;;; lexer.lisp -- Lexer for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Tokenizer for secd-lisp source code.
;;;; Converts source string into a list of tokens.

(in-package #:secd-lisp)

;;; Token types
(defstruct (token (:constructor make-token (type value line column)))
  "Represents a single token from the source."
  (type nil :type keyword)
  (value nil)
  (line 0 :type fixnum)
  (column 0 :type fixnum))

;;; Token types
;; :left-paren    - (
;; :right-paren   - )
;; :quote         - '
;; :quasiquote    - `
;; :unquote       - ,
;; :splice        - ,@
;; :symbol        - symbol name
;; :integer       - integer literal
;; :float         - float literal
;; :string        - string literal
;; :character     - character literal
;; :boolean       - #t or #f
;; :nil           - nil
;; :keyword       - :keyword
;; :dot           - .
;; :eof           - end of file

;;; Lexer state
(defstruct (lexer (:constructor %make-lexer))
  "State of the lexer during tokenization."
  (input "" :type string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum))

;;; Create a new lexer
(defun make-lexer (input)
  "Create a new lexer for the given input string."
  (%make-lexer :input input :position 0 :line 1 :column 1))

;;; Peek at current character
(defun lexer-peek (lexer &optional (offset 0))
  "Peek at the character at current position + offset."
  (let ((pos (+ (lexer-position lexer) offset)))
    (when (< pos (length (lexer-input lexer)))
      (char (lexer-input lexer) pos))))

;;; Advance position
(defun lexer-advance (lexer)
  "Advance the lexer position by one character."
  (let ((ch (lexer-peek lexer)))
    (incf (lexer-position lexer))
    (if (eql ch #\Newline)
        (progn
          (incf (lexer-line lexer))
          (setf (lexer-column lexer) 1))
        (incf (lexer-column lexer)))
    ch))

;;; Skip whitespace and comments
(defun lexer-skip-whitespace (lexer)
  "Skip whitespace and comments."
  (loop while (< (lexer-position lexer) (length (lexer-input lexer)))
        do (let ((ch (lexer-peek lexer)))
             (cond
               ;; Whitespace
               ((member ch '(#\Space #\Tab #\Newline #\Return))
                (lexer-advance lexer))
               ;; Comment (; to end of line)
               ((eql ch #\;)
                (loop while (and (< (lexer-position lexer) (length (lexer-input lexer)))
                                 (not (eql (lexer-peek lexer) #\Newline)))
                      do (lexer-advance lexer)))
               ;; Not whitespace
               (t (return nil))))))

;;; Read a symbol or keyword
(defun lexer-read-symbol (lexer)
  "Read a symbol or keyword from the input."
  (let ((start (lexer-position lexer))
        (start-line (lexer-line lexer))
        (start-col (lexer-column lexer)))
    ;; Read characters until whitespace or special character
    (loop while (< (lexer-position lexer) (length (lexer-input lexer)))
          for ch = (lexer-peek lexer)
           while (and ch
                     (not (member ch '(#\Space #\Tab #\Newline #\Return
                                        #\( #\) #\[ #\] #\{ #\}
                                        #\' #\" #\; #\, #\`))))
          do (lexer-advance lexer))
    (let ((value (subseq (lexer-input lexer) start (lexer-position lexer))))
      ;; Check if it's a keyword (starts with :)
      (if (and (> (length value) 0) (eql (char value 0) #\:))
          (make-token :keyword (intern (subseq value 1) "KEYWORD") start-line start-col)
          (make-token :symbol (intern (string-upcase value) "SECD-LISP") start-line start-col)))))

;;; Read a number
(defun lexer-read-number (lexer)
  "Read a number from the input. Supports decimal, hex (0x..), and binary (0b..)."
  (let ((start (lexer-position lexer))
        (start-line (lexer-line lexer))
        (start-col (lexer-column lexer))
        (has_dot nil)
        (radix 10))
    ;; Consume an optional leading minus so negative literals work
    (when (and (eql (lexer-peek lexer) #\-)
               (digit-char-p (lexer-peek lexer 1)))
      (lexer-advance lexer))
    ;; Detect 0x / 0X (hex) or 0b / 0B (binary) prefix; only if the next
    ;; character is itself a hex digit / 0 or 1 so that plain "0" still
    ;; reads as zero and "08" stays decimal.
    (when (and (eql (lexer-peek lexer) #\0)
               (member (lexer-peek lexer 1) '(#\x #\X #\b #\B)))
      (let ((p (lexer-peek lexer 1)))
        (cond ((or (eql p #\x) (eql p #\X)) (setq radix 16))
              (t (setq radix 2)))
        (lexer-advance lexer)
        (lexer-advance lexer)))
    ;; Read digits and optional dot (for decimal floats; hex/binary are int-only)
    (loop while (< (lexer-position lexer) (length (lexer-input lexer)))
          for ch = (lexer-peek lexer)
          while (or (digit-char-p ch radix)
                    (and (eql radix 10)
                         (eql ch #\.) (not has-dot) (setq has-dot t)))
          do (lexer-advance lexer))
    (let ((value (subseq (lexer-input lexer) start (lexer-position lexer))))
      (if has_dot
          (make-token :float (read-from-string value) start-line start-col)
          (make-token :integer
                      (cond ((= radix 10) (parse-integer value))
                            (t (parse-integer
                                (subseq value
                                        (if (char= (char value 0) #\-) 3 2))
                                :radix radix)))
                      start-line start-col)))))

;;; Read a string
(defun lexer-read-string (lexer)
  "Read a string literal from the input."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer))
        (chars nil))
    ;; Skip opening quote
    (lexer-advance lexer)
    ;; Read until closing quote
    (loop while (< (lexer-position lexer) (length (lexer-input lexer)))
          for ch = (lexer-advance lexer)
          until (eql ch #\")
          do (if (eql ch #\\)
                 ;; Escape sequence
                 (let ((escaped (lexer-advance lexer)))
                   (push (case escaped
                           (#\n #\Newline)
                           (#\t #\Tab)
                           (#\r #\Return)
                           (#\\ #\\)
                           (#\" #\")
                           (t escaped))
                         chars))
                 (push ch chars)))
    (make-token :string (coerce (nreverse chars) 'string) start-line start-col)))

;;; Read a character literal
(defun lexer-read-character (lexer)
  "Read a character literal from the input."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer)))
    ;; Skip #\
    (lexer-advance lexer)
    (lexer-advance lexer)
    ;; Read character name
    (let ((name (make-string 0)))
      (loop while (< (lexer-position lexer) (length (lexer-input lexer)))
            for ch = (lexer-peek lexer)
            while (and ch (alphanumericp ch))
            do (progn
                 (setf name (concatenate 'string name (string ch)))
                 (lexer-advance lexer)))
      (let ((char (cond
                    ((string= name "space") #\Space)
                    ((string= name "tab") #\Tab)
                    ((string= name "newline") #\Newline)
                    ((string= name "return") #\Return)
                    ((= (length name) 1) (char name 0))
                    (t (error "Unknown character name: ~A" name)))))
        (make-token :character char start-line start-col)))))

;;; Read a boolean
(defun lexer-read-boolean (lexer)
  "Read a boolean literal from the input."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer)))
    ;; Skip #t or #f
    (lexer-advance lexer)
    (let ((ch (lexer-advance lexer)))
      (make-token :boolean (eql ch #\t) start-line start-col))))

;;; Read a byte-vector literal #(b0 b1 ... bn).
;;; Each element must be an integer literal in 0..255; returns a :byte-vector
;;; token whose value is a list of byte values.
(defun lexer-read-byte-vector (lexer)
  "Read a #(...) byte-vector literal from the input."
  (let ((start-line (lexer-line lexer))
        (start-col (lexer-column lexer)))
    ;; Skip #(
    (lexer-advance lexer)
    (lexer-advance lexer)
    (lexer-skip-whitespace lexer)
    (let ((bytes nil))
      (loop
        (let ((ch (lexer-peek lexer)))
          (cond
            ((null ch)
             (error "Unterminated byte-vector literal"))
            ((eql ch #\))
             (lexer-advance lexer)
             (return (make-token :byte-vector (nreverse bytes)
                                 start-line start-col)))
            ((member ch '(#\Space #\Tab #\Newline #\Return))
             (lexer-skip-whitespace lexer))
            (t
             (lexer-skip-whitespace lexer)
             (let ((token (next-token lexer)))
               (unless (eq (token-type token) :integer)
                 (error "Byte-vector elements must be integer literals, got ~A"
                        (token-type token)))
               (let ((val (token-value token)))
                 (when (or (< val 0) (> val 255))
                   (error "Byte-vector element out of range 0..255: ~A" val))
                 (push val bytes))))))))))

;;; Get next token
(defun next-token (lexer)
  "Get the next token from the lexer."
  (lexer-skip-whitespace lexer)
  (when (>= (lexer-position lexer) (length (lexer-input lexer)))
    (return-from next-token (make-token :eof nil (lexer-line lexer) (lexer-column lexer))))
  (let ((ch (lexer-peek lexer)))
    (cond
      ;; End of input
      ((null ch)
       (make-token :eof nil (lexer-line lexer) (lexer-column lexer)))
      ;; Parentheses
      ((eql ch #\()
       (lexer-advance lexer)
       (make-token :left-paren nil (lexer-line lexer) (1- (lexer-column lexer))))
      ((eql ch #\))
       (lexer-advance lexer)
       (make-token :right-paren nil (lexer-line lexer) (1- (lexer-column lexer))))
      ;; Quote
      ((eql ch #\')
       (lexer-advance lexer)
       (make-token :quote nil (lexer-line lexer) (1- (lexer-column lexer))))
      ;; Quasiquote
      ((eql ch #\`)
       (lexer-advance lexer)
       (make-token :quasiquote nil (lexer-line lexer) (1- (lexer-column lexer))))
      ;; Unquote or splice
      ((eql ch #\,)
       (lexer-advance lexer)
       (if (eql (lexer-peek lexer) #\@)
           (progn
             (lexer-advance lexer)
             (make-token :splice nil (lexer-line lexer) (- (lexer-column lexer) 2)))
           (make-token :unquote nil (lexer-line lexer) (1- (lexer-column lexer)))))
      ;; String
      ((eql ch #\")
       (lexer-read-string lexer))
      ;; Character
      ((and (eql ch #\#) (eql (lexer-peek lexer 1) #\\))
       (lexer-read-character lexer))
      ;; Boolean
      ((and (eql ch #\#) (member (lexer-peek lexer 1) '(#\t #\f)))
       (lexer-read-boolean lexer))
      ;; Byte-vector literal #( ... )
      ((and (eql ch #\#) (eql (lexer-peek lexer 1) #\())
       (lexer-read-byte-vector lexer))
      ;; Dot
      ((eql ch #\.)
       (lexer-advance lexer)
       (make-token :dot nil (lexer-line lexer) (1- (lexer-column lexer))))
      ;; Number
      ((or (digit-char-p ch)
           (and (eql ch #\-) (digit-char-p (lexer-peek lexer 1))))
       (lexer-read-number lexer))
      ;; Symbol or keyword
      (t
       (lexer-read-symbol lexer)))))

;;; Tokenize a string
(defun tokenize (input)
  "Tokenize a secd-lisp source string into a list of tokens."
  (let ((lexer (make-lexer input))
        (tokens nil))
    (loop
      (let ((token (next-token lexer)))
        (push token tokens)
        (when (eq (token-type token) :eof)
          (return (nreverse tokens)))))))
