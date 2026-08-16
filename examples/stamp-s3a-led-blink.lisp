;;;;; stamp-s3a-led-blink.lisp — WS2812 blink on the Stamp-S3A on-board LED.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Pure bytecode execution check, no USB. The Stamp-S3A's on-board RGB
;;;; (WS2812 NeoPixel) is on GPIO 21; this drives it with the reusable WS2812
;;;; driver, cycling red/green/blue. A visible colour cycle proves the VM
;;;; loaded and ran this program. Intended to be linked against the RELEASE
;;;; firmware (SECD_DEBUG_BUILD=0), which starts bytecode immediately with no
;;;; DTR/console gate.
;;;;
;;;;   (secd-lisp:secd-compile-file "examples/stamp-s3a-led-blink.lisp"
;;;;                                :target :stamp-s3a :entry "led-blink:main")

(defpackage :led-blink
  (:require (:ws2812 :refer :all)))

(defconstant +led-pin+ 21)  ; Stamp-S3A on-board WS2812 (per targets/boards)

(defun blink (rgb on-ms)
  (progn
    (rgb-show +led-pin+ (list rgb))
    (%sleep on-ms)
    (rgb-off +led-pin+ 1)
    (%sleep 250)))

(defun main ()
  (%gpio-init +led-pin+ :output)
  (loop
    (blink (list 64 0 0) 250)
    (blink (list 0 64 0) 250)
    (blink (list 0 0 64) 250)))