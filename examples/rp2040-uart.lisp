;;;; rp2040-uart.lisp — RP2040 UART echo example
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Configures the UART and echoes received bytes back.
;;;;
;;;; The baud rate must fit the target's fixnum range: RP2040-class VMs use
;;;; 12-bit fixnums (max 2047), so 115200 is not expressible from Lisp — this
;;;; example runs the UART at 9600. (Larger constants are rejected by the
;;;; compiler rather than silently truncated.)

;; Echo a byte back if it is non-zero.
(defun echo-byte (byte)
  (if (> byte 0)
      (%uart-write byte)
      0))

(defun main ()
  (%uart-init 9600)
  (loop
    (echo-byte (%uart-read))))
