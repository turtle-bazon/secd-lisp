;;;;; ble-keyboard.lisp — BLE HID keyboard from Lisp (NimBLE / S140).
;;;;
;;;;  Copyright (C) 2026 — License: GPL3
;;;;
;;;;  Brings up the BLE HID device from Lisp, sets a friendly name, then sends
;;;;  the key 'a' (HID usage 4) whenever the button is pressed. Pair from the
;;;;  OS Bluetooth settings; the device appears under the name set via
;;;;  %ble-name. On targets without a BLE stack %ble-init returns -1 and the
;;;;  program is a no-op.
;;;;
;;;;  Supported boards (pin wiring via reader conditionals):
;;;;    nRF52840 ProMicro clone (nice!nano): button P0.02 (active-low, internal
;;;;                                     pull-up; P0.31 is held low on most
;;;;                                     clones so it can't be used as input),
;;;;                                     LED on P0.15 (nice!nano blue, active-high)
;;;;    Stamp-S3A:                        button GPIO 10, WS2812 LED on 21
;;;;    Other boards:                    button GPIO 10, LED on GPIO 2
;;;;
;;;;  Compile / flash:
;;;;   (secd-lisp:secd-compile-file "examples/ble-keyboard.lisp"
;;;;                                :target :nrf52840-promicro
;;;;                                :entry "BLE-KEYBOARD:MAIN")
;;;;   (secd-lisp:secd-compile-file "examples/ble-keyboard.lisp"
;;;;                                :target :stamp-s3a
;;;;                                :entry "BLE-KEYBOARD:MAIN")

(defpackage :ble-keyboard)

(defconstant +usage-a+ 4)

;;; Button pin (active-low, wired to GND, internal pull-up).
#+nrf52840-promicro (defconstant +button-pin+ 2)
#-nrf52840-promicro (defconstant +button-pin+ 10)

;;; LED pin is board-specific; override per target with reader conditionals.
#+nrf52840-promicro (defconstant +led+ 15)   ; nice!nano blue LED, P0.15, active-high
#+stamp-s3a         (defconstant +led+ 21)   ; WS2812 data pin
#-(or nrf52840-promicro stamp-s3a) (defconstant +led+ 2)
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
