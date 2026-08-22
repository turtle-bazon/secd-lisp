;;;; bignum-factorial.lisp — arbitrary-precision factorial for the
;;;; Cardputer (and every other supported board).
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Computes n! exactly using the %bn-* primitives (arbitrary-precision
;;;; integers backed by byte-vector magnitudes; see secd-machine
;;;; primitives.cpp). Constants larger than the VM's 12-bit fixnums are
;;;; emitted by the compiler as boxed wide constants (LDCW), and %bn-cmp
;;;; returns -1/0/1 as a plain fixnum.
;;;;
;;;; Tiered by board capability so small-RAM boards still participate:
;;;;   every board            170!   (307 digits)
;;;;   mid-memory boards      5000!  (~16308 digits)
;;;;   big-arena boards      20000!  (~73440 digits, needs ~150 KB arena)
;;;;
;;;; Build examples:
;;;;   ./build/secd-lisp examples/bignum-factorial.lisp \
;;;;       --target stamp-s3a --entry "BN-FACTORIAL:MAIN"

(defpackage :bn-factorial
  (:require (:core :refer :all)))

;;; Loop expressed recursively: while I > 1, ACC *= I; I -= 1.
;;; %bn-cmp returns a fixnum, so the builtin > tests it.
(defun fact-iter (i acc)
  (if (> (%bn-cmp i 1) 0)
      (fact-iter (%bn-sub i 1) (%bn-mul acc i))
      acc))

(defun fact (n)
  (fact-iter n (%bn-from-string "1")))

;;; Compute n!, print the label and the full decimal expansion.
(defun report (label n)
  (progn
    (print label)
    (let ((r (fact n)))
      (print (%bn-to-string r))
      nil)))

(defun main ()
  ;; Tier 1: every board.
  (report "170!" 170)
  ;; Tier 2: boards with >=32 KB byte arena.
  #+(or rp2040-pico rp2040-zero rp2350-zero rp2350-beetle
        esp32c3-supermini esp32s2 lolin-s2-mini black-pill-f401)
  (report "5000!" 5000)
  ;; Tier 3: boards with a large arena (ESP32-S3 family).
  #+(or stamp-s3a esp32s3-devkit lolin-s3-mini)
  (report "20000!" 20000)
  0)
