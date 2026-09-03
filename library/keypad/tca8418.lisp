;;;; keypad/tca8418.lisp — TCA8418 I2C keypad controller driver.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; The TCA8418 is an I2C keypad controller that scans a matrix of up to
;;;; 8 rows x 10 columns and reports key events through a FIFO. Key events
;;;; carry a press/release bit (bit 7) and a key number (0..79 in the lower
;;;; 7 bits). The driver here exposes:
;;;;
;;;;   (tca8418:init addr)         — configure the chip for a 7x8 matrix
;;;;   (tca8418:event-count addr)  — pending events in the FIFO (low 4 bits)
;;;;   (tca8418:event addr)        — read one event; returns (press . key) or nil
;;;;
;;;; Register names are exported as constants prefixed with TCA8418:+
;;;; (e.g. TCA8418:+CFG+, TCA8418:+EVENT+).
;;;;
;;;; Use from a program with:
;;;;   (defpackage :my-app (:require (:keypad/tca8418)))

(defpackage :tca8418
  (:export init event-count event)
  ;;; Register addresses (see TCA8418 datasheet).
  (:export CFG INT-STAT COUNT EVENT INT-EN-1 INT-EN-2 INT-EN-3
           KP-GPIO-1 KP-GPIO-2 GPI-EM-1 GPI-EM-2 GPI-EM-3
           DIR-1 DIR-2 DIR-3 INT-LVL-1 INT-LVL-2 INT-LVL-3
           GPIO-INT-STAT-1 GPIO-INT-STAT-2 GPIO-INT-STAT-3))

(defconstant CFG 1)                  ; 0x01  configuration
(defconstant INT-STAT 2)             ; 0x02  interrupt status
(defconstant COUNT 3)                ; 0x03  key-lock / event count
(defconstant EVENT 4)                ; 0x04  key event FIFO
(defconstant GPIO-INT-STAT-1 17)     ; 0x11  GPIO interrupt status 1..3
(defconstant GPIO-INT-STAT-2 18)
(defconstant GPIO-INT-STAT-3 19)
(defconstant INT-EN-1 26)            ; 0x1A  GPIO interrupt enable 1..3
(defconstant INT-EN-2 27)
(defconstant INT-EN-3 28)
(defconstant KP-GPIO-1 29)           ; 0x1D  keypad row select
(defconstant KP-GPIO-2 30)           ; 0x1E  keypad column select
(defconstant GPI-EM-1 32)            ; 0x20  GPI event mode 1..3
(defconstant GPI-EM-2 33)
(defconstant GPI-EM-3 34)
(defconstant DIR-1 35)               ; 0x23  GPIO data direction 1..3
(defconstant DIR-2 36)
(defconstant DIR-3 37)
(defconstant INT-LVL-1 38)           ; 0x26  GPIO edge/level 1..3
(defconstant INT-LVL-2 39)
(defconstant INT-LVL-3 40)

(defun reg-write (addr reg value)
  (%i2c-write addr (list reg value)))

(defun reg-read (addr reg nbytes)
  (%i2c-write-read addr (list reg) nbytes))

;;; Drain the key-event FIFO (KEY_EVENT_A reads 0 once empty).
(defun drain (addr)
  (if (= (vref (reg-read addr EVENT 1) 0) 0)
      t
      (drain addr)))

;;; Begin/flush: configure for a 7x8 matrix and clear pending state.
(defun init (addr)
  ;; All GPIOs input, key-event mode, falling edge, interrupt-enabled.
  (reg-write addr DIR-1 0)
  (reg-write addr DIR-2 0)
  (reg-write addr DIR-3 0)
  (reg-write addr GPI-EM-1 255)
  (reg-write addr GPI-EM-2 255)
  (reg-write addr GPI-EM-3 255)
  (reg-write addr INT-LVL-1 0)
  (reg-write addr INT-LVL-2 0)
  (reg-write addr INT-LVL-3 0)
  (reg-write addr INT-EN-1 255)
  (reg-write addr INT-EN-2 255)
  (reg-write addr INT-EN-3 255)
  ;; matrix(7,8): 7 rows + 8 columns.
  (reg-write addr KP-GPIO-1 127)
  (reg-write addr KP-GPIO-2 255)
  ;; flush: drain events, clear GPIO status + INT_STAT.
  (drain addr)
  (reg-read addr GPIO-INT-STAT-1 1)
  (reg-read addr GPIO-INT-STAT-2 1)
  (reg-read addr GPIO-INT-STAT-3 1)
  (reg-write addr INT-STAT 3))

;;; Pending event count (low 4 bits of KEY_LCK_EC).
(defun event-count (addr)
  (mod (vref (reg-read addr COUNT 1) 0) 16))

;;; Read one event: returns (press . key-number) or nil if FIFO is empty
;;; or the key number is out of range.
(defun event (addr)
  (let ((ev (vref (reg-read addr EVENT 1) 0)))
    (let ((press (>= ev 128))
          (kn (- (mod ev 128) 1)))
      (if (< kn 0)
          nil
          (if (>= kn 80)
              nil
              (cons press kn))))))
