;;;; esp32c3-blink-test.lisp — ESP32-C3 console + GPIO test for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Blinks board LED pin 8 a fixed number of times; prints the remaining
;;;; count each iteration as numeric console output (USB-Serial/JTAG console).

(defun led-init ()
  (%gpio-init 8 :output))

(defun led-on ()
  (%gpio-write 8 1))

(defun led-off ()
  (%gpio-write 8 0))

(defun blink (times delay)
  (if (> times 0)
      (progn
        (led-on)
        (print times)
        (%sleep delay)
        (led-off)
        (%sleep delay)
        (blink (- times 1) delay))
      0))

(defun main ()
  (led-init)
  (blink 10 500)
  0)