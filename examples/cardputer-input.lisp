;;;;; cardputer-input.lisp -- Cardputer-ADV keyboard + IMU to USB HID.
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
;;;; The BMI270 will not run until a 8192-byte Bosch blob is uploaded to it;
;;;; its gyro X/Y rate is scaled into relative mouse deltas, so rotating the
;;;; board moves the cursor. The byte-vector trick: the register byte 0x5E
;;;; rides as element 0 of the literal.
;;;;
;;;; The TCA8418 and BMI270 drivers live in library/ (keypad/tca8418.lisp,
;;;; imu/bmi270.lisp); this file is the Cardputer-ADV board wiring + the app.
;;;; Every driver call takes the I2C bus index (0..1) first: the internal bus
;;;; is initialized below and *i2c-bus* holds the returned index.
;;;;
;;;; Build with:
;;;;   (secd-lisp:secd-compile-file "examples/cardputer-input.lisp"
;;;;                                :target :stamp-s3a
;;;;                                :entry "CARDPUTER-INPUT:MAIN")

(defpackage :cardputer-input
  (:require (:core :refer :all)
            (:keypad/tca8418)
            (:imu/bmi270)))

;;; Internal I2C bus (Cardputer-ADV): SDA=8, SCL=9, 400 kHz. %i2c-init picks
;;; the first free controller and returns its bus index (0 or 1); this runs
;;; at program start, before main.
(defconstant +i2c-sda+ 8)
(defconstant +i2c-scl+ 9)
(defconstant +i2c-khz+ 400)
(defvar *i2c-bus* (%i2c-init +i2c-sda+ +i2c-scl+ +i2c-khz+))

;;; Board wiring: devices on the internal I2C bus.
(defconstant +kbd-addr+ 0x34)    ; TCA8418 keypad controller
(defconstant +imu-addr+ 0x69)    ; BMI270 IMU (0x68 does not ACK)

;;; HID modifier bits (Ctrl=1, Shift=2, Alt=4), modifier keys and special
;;; keys. The special keys below are HID usage codes, matching what the
;;; M5Cardputer keyboard stores as value_first.
(defconstant +mod-ctrl+ 1)
(defconstant +mod-shift+ 2)
(defconstant +mod-alt+ 4)
(defconstant +key-none+ 0x00)
(defconstant +key-opt+ 0x00)
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
;;; already HID usage codes (F1=58..F12=69, arrows, ESC, DEL); 0 = none.
(defconstant +keymap3+
  #(  41 58 59 60 61 62 63 64 65 66 67 68 69 76 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 255 0 0 0
  0 0 0 0 0 0 0 82 0 0 0 0 0 0 0 0
  0 0 0 0 80 81 79 0))

;;; ASCII -> HID usage table from the M5Cardputer library. Only the
;;; unshifted entries are reached here (the base layer always sends
;;; value_first plus the held modifier bits); shifted symbols like '!' are
;;; produced by the host from shift + the base usage.
(defconstant +asciimap+
  #(  0 0 0 0 0 0 0 0 42 43 40 0 0 0 0 0
  76 0 0 0 0 0 0 0 0 0 0 41 0 0 0 0
  44 158 180 160 161 162 164 52 166 167 165 174 54 45 55 56
  39 30 31 32 33 34 35 36 37 38 179 51 182 46 183 184
  159 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
  147 148 149 150 151 152 153 154 155 156 157 47 49 48 163 173
  53 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
   19 20 21 22 23 24 25 26 27 28 29 175 177 176 181 0))

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
    (let ((press (tca8418:event-press ev))
          (kn (tca8418:event-key ev)))
      (if (< kn 0)
          nil
          (if (>= kn 80)
              nil
              (let ((r0 (/ kn 10))
                    (c0 (mod kn 10)))
                ;; TCA8418 remap to the Cardputer matrix (see TCA8418.cpp).
                (let ((row (mod (+ c0 4) 4))
                      (col (+ (* r0 2) (if (> c0 3) 1 0))))
                  (handle-key (+ (* row 14) col) press mods fn))))))))

;;; Drain any pending events (KEY_LCK_EC low 4 bits = FIFO depth).
(defun kbd-scan (mods fn)
  (let ((count (tca8418:count *i2c-bus* +kbd-addr+)))
    (if (> count 0)
        (progn
          (kbd-event mods fn)
          (kbd-scan mods fn))
        nil)))

;;; --- main ----------------------------------------------------------

(defun main ()
  ;; USB device identity (strings encoded to UTF-16LE by to-c-string).
  (%usb-vid 0x1234)
  (%usb-pid 0x5678)
  (%usb-manufacturer (to-c-string "M5Stack"))
  (%usb-product (to-c-string "Cardputer"))
  (%usb-serial (to-c-string "000000000001"))
  (%usb-init)
  (%usb-hid-add)                  ; HID keyboard interface
  (%usb-mouse-add)                ; HID mouse interface
  (%usb-start)                    ; freeze + enumerate
  (tca8418:init *i2c-bus* +kbd-addr+)
  (bmi270:init *i2c-bus* +imu-addr+)
  (let ((mods (make-vector 1))    ; held Ctrl/Shift/Alt bits
        (fn (make-vector 1))      ; FN-layer flag
        (tick 0))                 ; loop counter
    (loop
      (kbd-scan mods fn)
      (setf tick (+ tick 1))
      ;; Roll -> pointer X, pitch -> pointer Y.
      (%hid-mouse 0 (bmi270:word-scale *i2c-bus* +imu-addr+ bmi270:+gyr-y-lsb+)
                  (bmi270:word-scale *i2c-bus* +imu-addr+ bmi270:+gyr-x-lsb+) 0)
      (%sleep 10))))

