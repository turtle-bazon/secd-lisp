;;;; cardputer-imu-scan.lisp -- Identify the Cardputer-ADV IMU over I2C.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; The keyboard (TCA8418 @ 0x34) works, so the I2C bus is fine. The mouse
;;;; path reads 0 for chip-id, so nothing ACKs at the assumed address. This
;;;; diagnostic probes a curated list of candidate IMU addresses (flat, no
;;;; deep recursion, with yields between probes so the watchdog stays fed)
;;;; and reports, for each, register 0 (chip-id) and register 0x75 (WHO_AM_I):
;;;;   BMI270  -> reg0    = 0x24 (36)
;;;;   MPU6886 -> reg0x75 = 0x19 (25)
;;;;   QMI8658 -> reg0    = 0x05 (5)
;;;;   MPU6050 -> reg0x75 = 0x68 (104)
;;;;   <other> -> whatever it returns
;;;;
;;;; Output goes to the CDC console (one fixnum per line). Single prints are
;;;; easily dropped until the host opens the port, so markers are repeated and
;;;; each probe prints its result twice. Tags:
;;;;   100..     alive / i2c-up markers (repeated)
;;;;   700  addr  reg0  reg0x75   == 0x34 keyboard baseline
;;;;   710  addr  reg0  reg0x75   == 0x68
;;;;   720  addr  reg0  reg0x75   == 0x69
;;;;   730  addr  reg0  reg0x75   == 0x6A
;;;;   740  addr  reg0  reg0x75   == 0x6B
;;;;   750  addr  reg0  reg0x75   == 0x19
;;;;   760  addr  reg0  reg0x75   == 0x18
;;;;   770  addr  reg0  reg0x75   == 0x77
;;;;
;;;; Build with:
;;;;   (secd-lisp:secd-compile-file "examples/cardputer-imu-scan.lisp"
;;;;                                :target :stamp-s3a :entry "IMU-SCAN:MAIN")

(defpackage :imu-scan)

(defconstant +i2c-sda+ 8)
(defconstant +i2c-scl+ 9)
(defconstant +i2c-khz+ 400)

;;; Read one byte from (addr, reg); NACK/error returns 0 (never throws).
(defun rbyte (addr reg)
  (vref (%i2c-write-read addr (list reg) 1) 0))

;;; Print N ten times so it survives the CDC "single print dropped" window.
(defun marker (n count)
  (if (= count 0)
      nil
      (progn (print n) (%sleep 50) (marker n (- count 1)))))

;;; Print tag, addr, reg0, reg0x75. Repeated twice so the values land.
(defun show (tag addr)
  (print tag)
  (print addr)
  (print (rbyte addr 0))
  (print (rbyte addr 117))
  (print tag)
  (print addr)
  (print (rbyte addr 0))
  (print (rbyte addr 117)))

(defun main ()
  (%usb-init)
  (%usb-start)
  (marker 100 10)                     ; alive
  (%i2c-init +i2c-sda+ +i2c-scl+ +i2c-khz+)
  (marker 200 10)                     ; i2c up
  (show 700 52) (%sleep 20)           ; 0x34 keyboard
  (show 710 104) (%sleep 20)          ; 0x68
  (show 720 105) (%sleep 20)          ; 0x69
  (show 730 106) (%sleep 20)          ; 0x6A
  (show 740 107) (%sleep 20)          ; 0x6B
  (show 750 25) (%sleep 20)           ; 0x19
  (show 760 24) (%sleep 20)           ; 0x18
  (show 770 119) (%sleep 20)          ; 0x77
  (marker 300 10))                     ; done
