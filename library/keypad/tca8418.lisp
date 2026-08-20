;;;;; tca8418.lisp — TCA8418 keypad controller driver (pure secd-lisp)
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; TCA8418 I2C keypad/GPIO controller. The device is configured in
;;;; key-event mode with a 7x8 matrix (mirroring Adafruit_TCA8418::begin +
;;;; matrix(7,8) + flush), then polled through its event FIFO — no interrupt
;;;; pin required: KEY_LCK_EC holds a count of pending events and each
;;;; KEY_EVENT_A read pops one.
;;;;
;;;; Every function takes the I2C bus index (returned by %i2c-init) and the
;;;; device address as parameters:
;;;;   (init bus addr)     bring-up: GPIOs input, key-event mode, matrix(7,8),
;;;;                       drain + clear status
;;;;   (count bus addr)    pending event count (0..15)
;;;;   (event bus addr)    pop one raw event byte (0x00 when empty)
;;;;   (event-press ev)    T if EV is a press event (bit7 set)
;;;;   (event-key ev)      key number 0..79 from EV (-1 when none)
;;;;
;;;; Use from a program with:
;;;;   (defpackage :my-program
;;;;     (:require (:keypad/tca8418)))
;;;;   (defvar *i2c* (%i2c-init 8 9 400))   ; bus 0 (or 1) once initialized
;;;;   (tca8418:init *i2c* 0x34)

(defpackage "TCA8418"
  (:export INIT COUNT EVENT EVENT-PRESS EVENT-KEY))

;; Register addresses (from the TCA8418 datasheet).
(defconstant +cfg+ 0x01)             ; configuration
(defconstant +int-stat+ 0x02)        ; interrupt status
(defconstant +count+ 0x03)           ; key-lock / event count
(defconstant +event+ 0x04)           ; key event FIFO
(defconstant +int-en-1+ 0x1A)        ; GPIO interrupt enable 1..3
(defconstant +int-en-2+ 0x1B)
(defconstant +int-en-3+ 0x1C)
(defconstant +kp-gpio-1+ 0x1D)       ; keypad row select
(defconstant +kp-gpio-2+ 0x1E)       ; keypad column select
(defconstant +gpi-em-1+ 0x20)        ; GPI event mode 1..3
(defconstant +gpi-em-2+ 0x21)
(defconstant +gpi-em-3+ 0x22)
(defconstant +dir-1+ 0x23)           ; GPIO data direction 1..3
(defconstant +dir-2+ 0x24)
(defconstant +dir-3+ 0x25)
(defconstant +int-lvl-1+ 0x26)       ; GPIO edge/level 1..3
(defconstant +int-lvl-2+ 0x27)
(defconstant +int-lvl-3+ 0x28)
(defconstant +gpio-int-stat-1+ 0x11) ; GPIO interrupt status 1..3
(defconstant +gpio-int-stat-2+ 0x12)
(defconstant +gpio-int-stat-3+ 0x13)

;; Write a single byte to register REG at device ADDR on bus BUS.
(defun reg-write (bus addr reg value)
  (%i2c-write bus addr (list reg value)))

;; Read N bytes starting at register REG from device ADDR on bus BUS.
(defun reg-read (bus addr reg n)
  (%i2c-write-read bus addr (list reg) n))

;; Drain the key-event FIFO (KEY_EVENT_A reads 0 once empty).
(defun drain (bus addr)
  (if (= (vref (reg-read bus addr +event+ 1) 0) 0)
      t
      (drain bus addr)))

;; Begin/flush, mirroring Adafruit_TCA8418::begin + matrix(7,8) + flush.
(defun init (bus addr)
  (reg-write bus addr +dir-1+ 0)     ; all GPIOs input
  (reg-write bus addr +dir-2+ 0)
  (reg-write bus addr +dir-3+ 0)
  (reg-write bus addr +gpi-em-1+ 255) ; key-event mode
  (reg-write bus addr +gpi-em-2+ 255)
  (reg-write bus addr +gpi-em-3+ 255)
  (reg-write bus addr +int-lvl-1+ 0) ; falling edge
  (reg-write bus addr +int-lvl-2+ 0)
  (reg-write bus addr +int-lvl-3+ 0)
  (reg-write bus addr +int-en-1+ 255) ; interrupt-enabled
  (reg-write bus addr +int-en-2+ 255)
  (reg-write bus addr +int-en-3+ 255)
  (reg-write bus addr +kp-gpio-1+ 127) ; matrix(7,8): 7 rows
  (reg-write bus addr +kp-gpio-2+ 255) ; + 8 columns
  (drain bus addr)                   ; flush
  (reg-read bus addr +gpio-int-stat-1+ 1)
  (reg-read bus addr +gpio-int-stat-2+ 1)
  (reg-read bus addr +gpio-int-stat-3+ 1)
  (reg-write bus addr +int-stat+ 3))

;; Pending key-event count (KEY_LCK_EC low 4 bits = FIFO depth).
(defun count (bus addr)
  (mod (vref (reg-read bus addr +count+ 1) 0) 16))

;; Pop one raw key event byte (0x00 when the FIFO is empty).
(defun event (bus addr)
  (vref (reg-read bus addr +event+ 1) 0))

;; T if EV is a press event (bit7 set = press, clear = release).
(defun event-press (ev)
  (>= ev 128))

;; Key number 0..79 from a raw event byte EV (-1 when the FIFO reported
;; empty). The number is the keypad matrix position 1..80, minus one.
(defun event-key (ev)
  (- (mod ev 128) 1))
