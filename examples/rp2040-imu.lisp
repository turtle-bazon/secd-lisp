;;;;; rp2040-imu.lisp -- RP2040 (Pico / Zero) BMI270 inertial sensor over
;;;;; the default I2C0 bus, driving a USB HID mouse pointer.
;;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;;
;;;; Hardware: any RP2040 board. The BMI270 is on the default I2C0 pins
;;;; (SDA=GP4, SCL=GP5 on the Pico; SDA=GP4, SCL=GP5 on the Waveshare
;;;; RP2040-Zero too), 400 kHz fast mode, address 0x68. USB is brought
;;;; up from Lisp as a composite device carrying the CDC console plus
;;;; a HID mouse.
;;;;;
;;;; The BMI270 driver (init, reg-read, reg-write, the 8192-byte Bosch
;;;; config blob) lives in library/imu/bmi270.lisp. This file is the
;;;; board wiring + the air-mouse app.
;;;;;
;;;; Pointer motion: the gyro X/Y angular rate is turned into relative
;;;; mouse deltas, so rotating the board moves the cursor.
;;;; Raw sensor words print to the CDC console, decimated to every 10th
;;;; loop so the stream stays readable.
;;;;;
;;;; Build with:
;;;;   make rp2040-imu

(defpackage :rp2040-imu
  (:require (:imu/bmi270 :as :bmi270)))

;; I2C0 pins and the BMI270's bus address.
(defconstant +imu-addr+ 0x68)
(defconstant +i2c-sda+ 0)
(defconstant +i2c-scl+ 5)
(defconstant +i2c-khz+ 400)         ; 400 kHz fast mode

;; Bus index returned by %i2c-init at runtime (0..1).
(defvar *i2c-bus* 0)

;;;; Clamp a fixnum to the signed int8 range %hid-mouse accepts.
(defun clamp-mouse (v)
  (if (> v 127)
      127
      (if (< v -127) -127 v)))

;;;; Signed 16-bit sensor word at REG, folded into the 12-bit fixnum range
;;;; and scaled by 1/64: r/64 = b0/64 + 4*b1, minus 1024 when the high byte
;;;; >= 128 (negative), then clamped to the mouse's int8 range. No shifts.
(defun word-scale (addr reg)
  (let ((v (bmi270:reg-read *i2c-bus* addr reg 2)))
    (let ((b0 (vref v 0))
          (b1 (vref v 1)))
      (clamp-mouse (- (+ (/ b0 64) (* 4 b1))
                      (if (>= b1 128) 1024 0))))))

(defun main ()
  (%usb-init)
  (%usb-mouse-add)
  (%usb-start)                    ; freeze + enumerate
  (setf *i2c-bus* (%i2c-init +i2c-sda+ +i2c-scl+ +i2c-khz+))
  (bmi270:init *i2c-bus* +imu-addr+)
  (let ((dec (make-vector 1)))    ; console decimation counter
    (loop
      ;; Roll around the board's Y axis -> pointer X, pitch around X ->
      ;; pointer Y (gyro Y is the roll axis, gyro X the pitch axis).
      (%hid-mouse 0 (word-scale +imu-addr+ bmi270:+GYR-Y-LSB+)
                  (word-scale +imu-addr+ bmi270:+GYR-X-LSB+) 0)
      (setf (vref dec 0) (+ (vref dec 0) 1))
      (if (= (vref dec 0) 10)
          (progn
            ;; Six-axis raw words (accel then gyro, each X/Y/Z), scaled by 1/64
            ;; and clamped so they fit the machine's fixnums, printed to the
            ;; CDC console every 10th pass.
            (print (word-scale +imu-addr+ bmi270:+ACC-X-LSB+))
            (print (word-scale +imu-addr+ (+ bmi270:+ACC-X-LSB+ 2)))
            (print (word-scale +imu-addr+ (+ bmi270:+ACC-X-LSB+ 4)))
            (print (word-scale +imu-addr+ bmi270:+GYR-X-LSB+))
            (print (word-scale +imu-addr+ (+ bmi270:+GYR-X-LSB+ 2)))
            (print (word-scale +imu-addr+ (+ bmi270:+GYR-X-LSB+ 4)))
            (setf (vref dec 0) 0))
          nil)
      (%sleep 10))))
