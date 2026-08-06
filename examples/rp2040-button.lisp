;;;; rp2040-button.lisp — RP2040 button + LED example
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Reads a button and controls LED.

;; Pin definitions
(defvar +led-pin+ 25)
(defvar +button-pin+ 0)

;; Initialize hardware
(defun init ()
  (%gpio-init +led-pin+ :output)
  (%gpio-init +button-pin+ :input))

;; Read button state
(defun button-pressed ()
  (= (%gpio-read +button-pin+) 1))

;; Main loop
(defun main ()
  (init)
  (loop
    (if (button-pressed)
        (%gpio-write +led-pin+ 1)
        (%gpio-write +led-pin+ 0))
    (%sleep 10)))
