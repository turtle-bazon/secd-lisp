;;;; rgb-blink.lisp — WS2812 RGB blink demo for the Waveshare RP2040-Zero
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Repeating colour blink pattern on the board's on-board NeoPixel using
;;;; the reusable WS2812 LED driver from library/led/ws2812.lisp.
;;;;
;;;; Pattern (repeats forever):
;;;;   Red    on 250 ms, off 250 ms,
;;;;   Green  on 250 ms, off 250 ms,
;;;;   Blue   on 250 ms, off 250 ms,
;;;;   pause 500 ms.
;;;;
;;;; The on-board NeoPixel on the Waveshare RP2040/RP2350 Zero is on pin 16
;;;; (pin 8 on the ESP32-C3 SuperMini).
;;;;
;;;; Package demo: the program declares its own package :rgb-blink;
;;;; (:require (:led/ws2812 :refer :all)) pulls in the WS2812 driver and
;;;; imports every name it exports (rgb-show, rgb-off) unqualified. The
;;;; entry point is chosen at build time with :entry "rgb-blink:main".

(defpackage :rgb-blink
  (:require (:led/ws2812 :refer :all)))

;; The on-board NeoPixel pin.
(defconstant +led-pin+ 16)

;; Light the pixel RGB for ON-MS, then turn it off for OFF-MS.
(defun blink (pin rgb on-ms off-ms)
  (progn
    (rgb-show pin (list rgb))
    (%sleep on-ms)
    (rgb-off pin 1)
    (%sleep off-ms)))

(defun main ()
  (%gpio-init +led-pin+ :output)
  (loop
    (blink +led-pin+ (list 64 0 0) 250 250)
    (blink +led-pin+ (list 0 64 0) 250 250)
    (blink +led-pin+ (list 0 0 64) 250 250)
    (%sleep 500)))
