;;;; rp2040-blink-loop.lisp — Infinite blink pattern for RP2040
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Blinks the built-in LED forever with pattern:
;;;;   short on (250ms), short off (250ms),
;;;;   short on (250ms), long off (500ms)

;; Initialize LED pin as output
(defun led-init ()
  (gpio-init 25 :output))

;; Short on: LED on for 250ms
(defun short-on ()
  (gpio-write 25 1)
  (sleep 250))

;; Short off: LED off for 250ms
(defun short-off ()
  (gpio-write 25 0)
  (sleep 250))

;; Long off: LED off for 500ms
(defun long-off ()
  (gpio-write 25 0)
  (sleep 500))

;; Main - infinite blink pattern
(defun main ()
  (led-init)
  (loop
    (short-on)
    (short-off)
    (short-on)
    (long-off)))

;; Run
(main)
