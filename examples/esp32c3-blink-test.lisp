;;;; esp32c3-blink-test.lisp — ESP32-C3 console + GPIO test for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Toggles GPIO 8 a fixed number of times (a plain GPIO smoke test).
;;;;
;;;; NOTE: on the C3 SuperMini, GPIO 8 is wired to an addressable WS2812 RGB
;;;; LED, so a level toggle is not visible as a blink. For a visible blink
;;;; use examples/portable-blink.lisp or rgb-blink-style programs.

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
        (%sleep delay)
        (led-off)
        (%sleep delay)
        (blink (- times 1) delay))
      0))

(defun main ()
  (led-init)
  (blink 10 500)
  0)