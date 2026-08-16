;;;;; esp32-hid-diag.lisp -- minimal ESP32-S3 USB HID diagnostic.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Bisects the Cardputer USB failure: this program brings up ONLY the USB
;;;; composite (console CDC + HID keyboard + HID mouse) with none of the
;;;; Cardputer I2C stack (no TCA8418, no BMI270, no %i2c-*). It then sends
;;;; a 'a' key every 500 ms on the HID keyboard interface.
;;;;
;;;; If this works on the device (kernel shows hid-generic + stays attached),
;;;; the failure lives in the cardputer-input.lisp program. If it also
;;;; disconnects ~1 s after enumerating, the ESP32 USB/HID path is at fault.
;;;;
;;;; Build with:
;;;;   (secd-lisp:secd-compile-file "examples/esp32-hid-diag.lisp"
;;;;                                :target :stamp-s3a
;;;;                                :entry "ESP32-HID-DIAG:MAIN")

(defpackage :esp32-hid-diag)

(defconstant +usage-a+ 4)

(defun main ()
  (%usb-init)
  (%usb-hid-add)                  ; HID keyboard interface
  (%usb-mouse-add)                ; HID mouse interface
  (%usb-start)                    ; freeze + enumerate
  (loop
    (%hid-key 0 +usage-a+)        ; press+release 'a'
    (%sleep 500)))