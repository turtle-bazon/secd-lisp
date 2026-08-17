;;;; core.lisp — core byte-vector kernel (pure secd-lisp)
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Small runtime helpers on top of the byte-vector opcodes
;;;; (LDV/VREF/VSTOR/MKV/LEN). The VM's `length` works on both byte-vectors
;;;; and lists; HAL reads (%i2c-read, %i2c-write-read) already return
;;;; byte-vectors, so byte data stays in vector-land end to end.
;;;;
;;;;    (range v from n)   n bytes from index `from` as a list
;;;;
;;;; Use from a program with:
;;;;    (defpackage :my-program (:require (:core :refer :all)))

(defpackage "CORE"
  (:export RANGE STRING->BYTES BYTES->STRING TO-C-STRING FROM-C-STRING))

;; N bytes of V starting at index FROM, as a list (bounded by length).
;; Returns nil when FROM is out of range or N is 0.
(defun range (v from n)
  (if (= n 0)
      nil
      (if (>= from (length v))
          nil
          (cons (vref v from) (range v (+ from 1) (- n 1))))))

;;; String <-> raw byte buffer conversions (the HAL/firmware boundary).
;;;
;;; In secd-lisp a string literal compiles to a UTF-8 byte-vector, so a string
;;; IS a byte-vector: vref/length work on it (byte level; ASCII == character
;;; level).
;;;
;;; STRING->BYTES / BYTES->STRING are the UTF-8 boundary: a Lisp string already
;;; is a UTF-8 byte-vector, so they are a pass-through (the seam where you
;;; would convert for a non-UTF-8 peripheral). Use these for UART/I2C/raw bytes.
(defun string->bytes (s) s)
(defun bytes->string (v) v)

;;; TO-C-STRING / FROM-C-STRING are the UTF-16LE seam for C/firmware wire
;;; formats (USB string descriptors). They are bound via `def-c-fun` to the
;;; UNIVERSAL firmware primitives `utf16-enc` / `utf16-dec` — present in every
;;; firmware image (not HAL-specific) — so any program using them runs
;;; unchanged on every machine.
;;;
;;; A Lisp string is a UTF-8 byte-vector; `to-c-string` converts it to a
;;; UTF-16LE byte-vector (what the firmware USB HAL consumes) and
;;; `from-c-string` does the reverse. Each takes exactly one argument (STR),
;;; declared here so the compiler checks the arity at every call site.

(def-c-fun (to-c-string utf16-enc) (str))
(def-c-fun (from-c-string utf16-dec) (str))
