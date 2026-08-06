;;;; blink.lisp — RP2040 LED blink example for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Blinks the built-in LED on RP2040 (GPIO 25) a fixed number of times.

;; Initialize LED pin as output
(defun led-init ()
  (%gpio-init 25 :output))

;; Turn LED on
(defun led-on ()
  (%gpio-write 25 1))

;; Turn LED off
(defun led-off ()
  (%gpio-write 25 0))

;; Blink LED (recursive countdown; no dotimes/loop-exit in the compiler yet)
(defun blink (times delay)
  (if (> times 0)
      (progn
        (led-on)
        (%sleep delay)
        (led-off)
        (%sleep delay)
        (blink (- times 1) delay))
      0))

;; Main program
(defun main ()
  (led-init)
  (blink 10 500))
