;;;;; ble-keyboard.lisp — BLE HID keyboard from Lisp (ESP32-S3 NimBLE).
;;;;
;;;;  Copyright (C) 2026 — License: GPL3
;;;;
;;;;  Brings up the BLE HID device from Lisp, sets a friendly name, then sends
;;;;  the key 'a' (HID usage 4) whenever the button on GPIO 10 is pressed.
;;;;  Pair from the OS Bluetooth settings; the device appears as the name set
;;;;  via %ble-name. On targets without a BLE stack %ble-init returns -1 and
;;;;  the program is a no-op.
;;;;
;;;;  Compile / flash:
;;;;   (secd-lisp:secd-compile-file "examples/ble-keyboard.lisp"
;;;;                                :target :stamp-s3a
;;;;                                :entry "BLE-KEYBOARD:MAIN")

(defpackage :ble-keyboard)

(defconstant +button-pin+ 10)
(defconstant +usage-a+ 4)

;;; LED pin is board-specific; override per target with reader conditionals.
#+stamp-s3a (defconstant +led+ 21)
#-stamp-s3a (defconstant +led+ 2)
(defconstant +led-on+ 1)

(defun main ()
  (%ble-init)
  (%ble-name "SECD Keyboard")
  (%gpio-init +button-pin+ :input)
  (%gpio-init +led+ :output)
  (loop
    (when (= (%gpio-read +button-pin+) 0)
      ;; mods = 0, then up to six 1-byte HID usages; here just 'a'.
      (%ble-key 0 #(4 0 0 0 0 0))
      (%gpio-write +led+ +led-on+)
      (%sleep 50)
      (%gpio-write +led+ 0)
      (%sleep 150))
    (%sleep 10)))
