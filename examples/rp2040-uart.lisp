;;;; rp2040-uart.lisp — RP2040 UART example
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; UART communication example.

;; Initialize UART
(defun uart-init ()
  (uart-write-string "SECD-Lisp on RP2040\n")
  (uart-write-string "==================\n"))

;; Send a number
(defun send-number (n)
  (uart-write-string "Number: ")
  (uart-write n)
  (uart-write-string "\n"))

;; Echo received data
(defun echo-loop ()
  (loop
    (when (serial-available)
      (let ((byte (serial-read)))
        (serial-write byte)))))

;; Main
(defun main ()
  (uart-init)
  (send-number 42)
  (echo-loop))

;; Run
(main)
