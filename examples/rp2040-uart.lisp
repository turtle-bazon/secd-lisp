;;;; rp2040-uart.lisp — RP2040 UART echo example
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; UART echo: configures the UART and echoes received bytes back.
;;;; (Note: the compiler's `let` binding-list support is pending, so this
;;;; example passes the byte through a helper function instead.)

;; Echo a byte back if it is non-zero.
(defun echo-byte (byte)
  (if (> byte 0)
      (%uart-write byte)
      0))

(defun main ()
  (%uart-init 115200)
  (loop
    (echo-byte (%uart-read))))