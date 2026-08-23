;;;; portable-blink.lisp — one blink example for every supported board.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; This file demonstrates secd-lisp reader conditionals (#+feature /
;;;; #-feature, with (and ...)/(or ...)/(not ...)) to adapt a single program
;;;; to different boards at compile time. Features come from the target's
;;;; .machine metadata:
;;;;
;;;;   - the board name            (blue-pill, stamp-s3a, rp2040-zero, ...)
;;;;   - every board "features"    (gpio, uart, i2c, sleep, millis)
;;;;   - derived LED markers       ("led" when board_pins.led_pin is set,
;;;;                                "ws2812" when board_pins.ws2812_pin is)
;;;;
;;;; Compile for any target with its own entry point:
;;;;   ./build/secd-lisp examples/portable-blink.lisp \
;;;;       --target blue-pill --entry "PORTABLE-BLINK:MAIN"
;;;;
;;;; Two LED kinds are handled:
;;;;   ws2812 boards drive the addressable RGB LED via library/led/ws2812;
;;;;   plain-gpio boards toggle a GPIO pin (active level chosen per board).
;;;;
;;;; Board table:
;;;;   blue-pill           GPIO  PC13 (=45) active-low
;;;;   black-pill-f401     GPIO  PC13 (=45) active-low
;;;;   seeed-xiao-samd21   GPIO  13          active-low
;;;;   rp2040-pico         GPIO  25
;;;;   rp2350-beetle       GPIO  25
;;;;   lolin-s2-mini       GPIO  15 (external LED; the S2 Mini has none)
;;;;   esp32c3-supermini   WS2812 8
;;;;   stamp-s3a           WS2812 21
;;;;   esp32s3-devkit      WS2812 48
;;;;   lolin-s3-mini       WS2812 47
;;;;   rp2040-zero         WS2812 16
;;;;   rp2350-zero         WS2812 16

(defpackage :portable-blink
  (:require (:core :refer :all)
            #+ws2812 (:led/ws2812 :refer :all)))

;;; --- per-board configuration -----------------------------------------

;; Plain-GPIO boards: pin and polarity.
#+blue-pill         (defconstant +led-pin+ 45)  ; PC13, active low
#+black-pill-f401   (defconstant +led-pin+ 45)  ; PC13, active low
#+seeed-xiao-samd21 (defconstant +led-pin+ 13)
#+rp2040-pico       (defconstant +led-pin+ 25)
#+rp2350-beetle     (defconstant +led-pin+ 25)
#+lolin-s2-mini     (defconstant +led-pin+ 15)  ; external LED on a free pin
#+nrf52840-supermini (defconstant +led-pin+ 15)

#+(or blue-pill black-pill-f401 seeed-xiao-samd21)
(defconstant +led-off+ 1)                ; active low: lit = 0
#-(or blue-pill black-pill-f401 seeed-xiao-samd21)
(defconstant +led-off+ 0)
#+(or blue-pill black-pill-f401 seeed-xiao-samd21)
(defconstant +led-on+ 0)
#-(or blue-pill black-pill-f401 seeed-xiao-samd21)
(defconstant +led-on+ 1)

;; WS2812 boards: data pin + pixel count for rgb-show/rgb-off.
#+esp32c3-supermini (defconstant +rgb-pin+ 8)
#+stamp-s3a         (defconstant +rgb-pin+ 21)
#+esp32s3-devkit    (defconstant +rgb-pin+ 48)
#+lolin-s3-mini     (defconstant +rgb-pin+ 47)
#+rp2040-zero       (defconstant +rgb-pin+ 16)
#+rp2350-zero       (defconstant +rgb-pin+ 16)
#+ws2812
(defconstant +rgb-pixels+ 1)

;;; --- driver selection -------------------------------------------------

;; WS2812 path: blink white <-> off through the ws2812 library.
#+ws2812
(defun led-init () nil)

#+ws2812
(defun led-on ()
  (rgb-show +rgb-pin+ +rgb-pixels+ 32 32 32))

#+ws2812
(defun led-off ()
  (rgb-off +rgb-pin+ +rgb-pixels+))

;; Plain GPIO path: initialize once, then toggle.
#-ws2812
(defun led-init ()
  (%gpio-init +led-pin+ :output))

#-ws2812
(defun led-on ()
  (%gpio-write +led-pin+ +led-on+))

#-ws2812
(defun led-off ()
  (%gpio-write +led-pin+ +led-off+))

;;; --- main --------------------------------------------------------------

(defun main ()
  (led-init)
  (loop
    (led-on)
    (%sleep 250)
    (led-off)
    (%sleep 250)))
