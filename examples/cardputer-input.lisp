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
  (:require (:keypad/tca8418)
            (:imu/bmi270)))

;; Internal I2C bus (Cardputer-ADV): SDA=8, SCL=9, 400 kHz.
(defconstant +i2c-sda+ 8)
(defconstant +i2c-scl+ 9)
(defconstant +i2c-khz+ 400)

;; Board wiring: devices on the internal I2C bus.
(defconstant +kbd-addr+ 52)         ; 0x34  TCA8418
(defconstant +imu-addr+ 105)        ; 0x69  BMI270 (0x68 does not ACK)

;;; HID modifier bits (Ctrl=1, Shift=2, Alt=4), modifier keys and special
;;; keys. The special keys below are HID usage codes, matching what the
;;; M5Cardputer keyboard stores as value_first.
(defconstant +mod-ctrl+ 1)
(defconstant +mod-shift+ 2)
(defconstant +mod-alt+ 4)
(defconstant +key-none+ 0)
(defconstant +key-opt+ 0)
(defconstant +key-fn+ 255)
(defconstant +key-ctrl+ 128)
(defconstant +key-shift+ 129)
(defconstant +key-alt+ 130)
(defconstant +key-backspace+ 42)     ; 0x2A
(defconstant +key-tab+ 43)           ; 0x2B
(defconstant +key-enter+ 40)         ; 0x28

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
  (let ((ev (tca8418:event +kbd-addr+)))
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
  (let ((count (tca8418:event-count +kbd-addr+)))
    (if (> count 0)
        (progn
          (print count)
          (kbd-event mods fn)
          (kbd-scan mods fn))
        nil)))

;;; --- main ----------------------------------------------------------

;; Print MARKER N times with a 50ms gap. The CDC console drops TX until the
;; host opens the port (DTR) and USB re-enumerates after %usb-start, so a
;; single print is easily lost; repeating a few times makes it land.
(defun marker (n count)
  (if (= count 0)
      nil
      (progn
        (print n)
        (%sleep 50)
        (marker n (- count 1)))))

(defun main ()
  (marker 100 10)                 ; alive: entered main
  (%usb-init)
  (marker 200 10)                 ; after usb init
  (%usb-hid-add)                  ; HID keyboard interface
  (marker 300 10)                 ; after hid add
  (%usb-mouse-add)                ; HID mouse interface
  (marker 400 10)                 ; after mouse add
  (%usb-start)                    ; freeze + enumerate
  (marker 000 10)                 ; after usb start
  (print (%i2c-init +i2c-sda+ +i2c-scl+ +i2c-khz+))
  (marker 111 10)                 ; after i2c init
  (tca8418:init +kbd-addr+)
  (marker 222 10)                 ; after kbd init
  (bmi270:init +imu-addr+)
  (marker 333 10)                 ; after bmi init
  (let ((mods (make-vector 1))    ; held Ctrl/Shift/Alt bits
        (fn (make-vector 1))      ; FN-layer flag
        (tick 0))                 ; heartbeat counter
    (loop
      (kbd-scan mods fn)
      (if (= 0 (mod tick 100))
          (print tick)
          nil)
      (setf tick (+ tick 1))
      ;; Roll -> pointer X, pitch -> pointer Y.
      (%hid-mouse 0 (bmi270:word-scale +imu-addr+ bmi270:+gyr-y-lsb+)
                  (bmi270:word-scale +imu-addr+ bmi270:+gyr-x-lsb+) 0)
      (if (= 0 (mod tick 10))
          (progn
            (print (bmi270:word-scale +imu-addr+ bmi270:+gyr-y-lsb+))
            (print (bmi270:word-scale +imu-addr+ bmi270:+gyr-x-lsb+)))
          nil)
      (%sleep 10))))
