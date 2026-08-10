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
;;;;    (defpackage :my-program (:require (:core)))

(defpackage "CORE"
  (:export RANGE))

;; N bytes of V starting at index FROM, as a list (bounded by length).
;; Returns nil when FROM is out of range or N is 0.
(defun range (v from n)
  (if (= n 0)
      nil
      (if (>= from (length v))
          nil
          (cons (vref v from) (range v (+ from 1) (- n 1))))))