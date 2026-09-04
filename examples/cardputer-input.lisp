;;;; cardputer-input.lisp -- Cardputer-ADV keyboard + IMU to USB HID.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Hardware: M5Stack Cardputer-ADV (ESP32-S3). Everything hangs off the
;;;; internal I2C bus (SDA=8, SCL=9, 400 kHz):
;;;;   - TCA8418 keypad controller at 0x34 (the physical keyboard)
;;;;   - BMI270 6-axis IMU at 0x69
;;;; USB is brought up from Lisp as a composite HID device: keyboard+mouse.
;;;;
;;;; The TCA8418 is polled through its event FIFO, so no interrupt pin is
;;;; needed: KEY_LCK_EC holds a count of pending key events, and each
;;;; KEY_EVENT_A read pops one. A press event (bit7 set) carries a key
;;;; number 1..80, remapped here to the Cardputer's 4x14 layout, then to a
;;;; HID usage via the keymap + asciimap tables below (parsed straight from
;;;; the M5Cardputer library so they stay byte-exact). Held ctrl/shift/alt
;;;; bits ride along as the HID modifier byte, so shift+1 types '!'. The FN
;;;; key layers to F-keys, arrows, ESC and DEL. Special keys (backspace,
;;;; tab, enter) are HID usages already and pass through directly.
;;;;
;;;; The BMI270 will not run until a 8192-byte Bosch blob is uploaded to it
;;;; (the blob lives in library/imu/bmi270.lisp); its gyro X/Y rate is
;;;; scaled into relative mouse deltas, so rotating the board moves the
;;;; cursor.
;;;;
;;;; The TCA8418 and BMI270 drivers live in library/ (keypad/tca8418.lisp,
;;;; imu/bmi270.lisp); this file is the Cardputer-ADV board wiring + the app.

(defpackage :cardputer-input
  (:require (:keypad/tca8418 :as :tca8418)
            (:imu/bmi270 :as :bmi270)))

;; Internal I2C bus (Cardputer-ADV): SDA=8, SCL=9, 400 kHz.
(defconstant +i2c-sda+ 8)
(defconstant +i2c-scl+ 9)
(defconstant +i2c-khz+ 400)

;; Bus index returned by %i2c-init at runtime (0..N-1). The libraries
;; take this as their first argument; main() sets it once and then
;; the rest of the program uses it for every transfer.
(defvar *i2c-bus* 0)

;; Board wiring: devices on the internal I2C bus.
(defconstant +kbd-addr+ 0x34)         ; TCA8418
(defconstant +imu-addr+ 0x69)         ; BMI270 (0x68 does not ACK)

;;; HID modifier bits (Ctrl=1, Shift=2, Alt=4), modifier keys and special
;;; keys. The special keys below are HID usage codes, matching what the
;;; M5Cardputer keyboard stores as value_first.
(defconstant +mod-ctrl+ 0b001)
(defconstant +mod-shift+ 0b010)
(defconstant +mod-alt+ 0b100)
(defconstant +key-none+ 0)
(defconstant +key-opt+ 0)
(defconstant +key-fn+ 0xFF)
(defconstant +key-ctrl+ 0x80)
(defconstant +key-shift+ 0x81)
(defconstant +key-alt+ 0x82)
(defconstant +key-backspace+ 0x2A)
(defconstant +key-tab+ 0x2B)
(defconstant +key-enter+ 0x28)

;;; Cardputer 4x14 keymap: value_first (base layer, row-major).
(defconstant +keymap+
  #(  96 49 50 51 52 53 54 55 56 57 48 45 61 42 43 113
  119 101 114 116 121 117 105 111 112 91 93 92 255 129 97 115
  100 102 103 104 106 107 108 59 39 40 128 0 130 122 120 99
  118 98 110 109 44 46 47 32))

;;; Cardputer 4x14 keymap: value_third (FN layer, row-major). Entries are
;;; already HID usage codes (F1=0x3A..F12=0x45, arrows, ESC, DEL); 0 = none.
(defconstant +keymap3+
  #(  0x29 0x3A 0x3B 0x3C 0x3D 0x3E 0x3F 0x40 0x41 0x42 0x43 0x44 0x45 0x4C 0x00 0x00
  0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0xFF 0x00 0x00 0x00
  0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x52 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
  0x00 0x00 0x00 0x00 0x50 0x51 0x4F 0x00))

;;; ASCII -> HID usage table from the M5Cardputer library. Only the
;;; unshifted entries are reached here (the base layer always sends
;;; value_first plus the held modifier bits); shifted symbols like '!' are
;;; produced by the host from shift + the base usage.
(defconstant +asciimap+
  #(  0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x2A 0x2B 0x28 0x00 0x00 0x00 0x00 0x00
  0x4C 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x29 0x00 0x00 0x00 0x00
  0x2C 0x9E 0xB4 0xA0 0xA1 0xA2 0xA4 0x34 0xA6 0xA7 0xA5 0xAE 0x36 0x2D 0x37 0x38
  0x27 0x1E 0x1F 0x20 0x21 0x22 0x23 0x24 0x25 0x26 0xB3 0x33 0xB6 0x2E 0xB7 0xB8
  0x9F 0x84 0x85 0x86 0x87 0x88 0x89 0x8A 0x8B 0x8C 0x8D 0x8E 0x8F 0x90 0x91 0x92
  0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9A 0x9B 0x9C 0x9D 0x2F 0x31 0x30 0xA3 0xAD
  0x35 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F 0x10 0x11 0x12
  0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0xAF 0xB1 0xB0 0xB5 0x00))

;;; One key event: update held modifiers / fn flag, or send a HID key tap.
;;; Pressing ctrl/shift/alt just toggles the modifier byte; the next
;;; printable tap carries it, so shift+1 produces '!' on the host.
(defun handle-key (idx press mods fn)
  (let ((value (vref +keymap+ idx)))
    (cond
      ((= value +key-fn+)
       (setf (vref fn 0) (if press 1 0)))
      ((= value +key-ctrl+)
       (setf (vref mods 0) (+ (vref mods 0) (if press +mod-ctrl+ (- 0 +mod-ctrl+)))))
      ((= value +key-shift+)
       (setf (vref mods 0) (+ (vref mods 0) (if press +mod-shift+ (- 0 +mod-shift+)))))
      ((= value +key-alt+)
       (setf (vref mods 0) (+ (vref mods 0) (if press +mod-alt+ (- 0 +mod-alt+)))))
      ((= value +key-opt+) nil)
      (press
       (if (= (vref fn 0) 1)
           ;; FN layer: value_third is already a HID usage code.
           (let ((usage (vref +keymap3+ idx)))
             (if (= usage +key-none+)
                 nil
                 (if (= usage +key-fn+)
                     nil
                     (%hid-key (vref mods 0) usage))))
           ;; Base layer: special keys pass through as HID usages;
           ;; everything else goes ASCII -> usage via the asciimap.
           (cond
             ((= value +key-backspace+) (%hid-key (vref mods 0) +key-backspace+))
             ((= value +key-tab+) (%hid-key (vref mods 0) +key-tab+))
             ((= value +key-enter+) (%hid-key (vref mods 0) +key-enter+))
             (t (let ((usage (vref +asciimap+ value)))
                  (if (= usage +key-none+) nil
                      (%hid-key (vref mods 0) usage))))))))
      (t nil)))

;;; Read one event, remap the TCA8418 key number to a Cardputer (row,col).
(defun kbd-event (mods fn)
  (let ((ev (tca8418:event *i2c-bus* +kbd-addr+)))
    (if (= ev nil)
        nil
        (let ((press (car ev))
              (kn (cdr ev)))
          (if (>= kn 80)
              nil
              (let ((r0 (/ kn 10))
                    (c0 (mod kn 10)))
                ;; TCA8418 remap to the Cardputer matrix.
                (let ((row (mod (+ c0 4) 4))
                      (col (+ (* r0 2) (if (> c0 3) 1 0))))
                  (handle-key (+ (* row 14) col) press mods fn))))))))

;;; Drain any pending events.
(defun kbd-scan (mods fn)
  (let ((count (tca8418:event-count *i2c-bus* +kbd-addr+)))
    (if (> count 0)
        (progn
          (print count)
           (kbd-event mods fn)
           (kbd-scan mods fn))
        nil)))

;;;; Clamp a fixnum to the signed int8 range %hid-mouse accepts.
(defun clamp-mouse (v)
  (if (> v 127)
      127
      (if (< v -127) -127 v)))

;;;; Signed 16-bit sensor word at REG, folded into the 12-bit fixnum range
;;;; and scaled by 1/64: r/64 = b0/64 + 4*b1, minus 1024 when the high byte
;;;; >= 128 (negative), then clamped to the mouse's int8 range. No shifts.
(defun word-scale (bus addr reg)
  (let ((v (bmi270:reg-read bus addr reg 2)))
    (let ((b0 (vref v 0))
          (b1 (vref v 1)))
      (clamp-mouse (- (+ (/ b0 64) (* 4 b1))
                      (if (>= b1 128) 1024 0))))))

(defun main ()
  (%usb-init)
  (%usb-vid 0x1209)                          ; placeholder VID
  (%usb-pid 0x4D35)                          ; placeholder PID ("MS")
  (%usb-vendor #(0x4D 0x35 0x53 0x54 0x41 0x43 0x4B))    ; "M5STACK"
  (%usb-product #(0x43 0x41 0x52 0x44 0x50 0x55 0x54 0x45 0x52))    ; "CARDPUTER"
  (%usb-hid-keyboard-add)        ; HID keyboard interface
  (%usb-hid-mouse-add)            ; HID mouse interface
  (%usb-start)                    ; freeze + enumerate
  (setf *i2c-bus* (%i2c-init +i2c-sda+ +i2c-scl+ +i2c-khz+))
  (tca8418:init *i2c-bus* +kbd-addr+)
  (bmi270:init *i2c-bus* +imu-addr+)
  (let ((mods (make-vector 1))    ; held Ctrl/Shift/Alt bits
        (fn (make-vector 1))      ; FN-layer flag
        (tick 0))                 ; heartbeat counter
    (loop
      (kbd-scan mods fn)
      (setf tick (+ tick 1))
      ;; Roll -> pointer X, pitch -> pointer Y.
      (%hid-mouse 0 (word-scale *i2c-bus* +imu-addr+ bmi270:+GYR-Y-LSB+)
                  (word-scale *i2c-bus* +imu-addr+ bmi270:+GYR-X-LSB+) 0)
      (if (= 0 (mod tick 10))
          (progn
            (print (word-scale *i2c-bus* +imu-addr+ bmi270:+GYR-Y-LSB+))
            (print (word-scale *i2c-bus* +imu-addr+ bmi270:+GYR-X-LSB+)))
          nil)
      (%sleep 10))))
