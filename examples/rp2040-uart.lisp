;;;; rp2040-uart.lisp — RP2040 UART echo example
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Configures the UART and echoes received bytes back.
;;;;
;;;; The baud rate exceeds the VM's 12-bit immediate fixnum range, so it is
;;;; emitted as a boxed wide constant (LDCW): the compiler stores the value,
;;;; the VM materializes it as a BIGNUM, and %uart-init decodes either kind.

;; Echo a byte back if it is non-zero.
(defun echo-byte (byte)
  (if (> byte 0)
      (%uart-write byte)
      0))

(defun main ()
  (%uart-init 115200)
  (loop
    (echo-byte (%uart-read))))
