;;;;; esp32-hid-diag-kbd.lisp -- ESP32-S3 USB HID diagnostic, keyboard only.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Same as esp32-hid-diag.lisp but WITHOUT the HID mouse interface. This
;;;; mirrors the RP2040 usb-keyboard example (console CDC + HID keyboard
;;;; only), which works on the Pico. Useful to isolate keyboard from mouse
;;;; while debugging.
;;;;
;;;; Build with:
;;;;   (secd-lisp:secd-compile-file "examples/esp32-hid-diag-kbd.lisp"
;;;;                                :target :stamp-s3a
;;;;                                :entry "ESP32-HID-DIAG-KBD:MAIN")

(defpackage :esp32-hid-diag-kbd)

(defconstant +usage-a+ 4)

(defun main ()
  (%usb-init)
  (%usb-hid-keyboard-add)                  ; HID keyboard interface ONLY
  (%usb-start)
  (loop
    (%hid-key 0 +usage-a+)
    (%sleep 500)))