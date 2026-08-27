;;;;; usb-keyboard.lisp — USB device exposed from Lisp (CherryUSB).
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Runs on any HID-capable board (e.g. RP2040 / RP2350 / nRF52840). USB
;;;; composite is brought up from Lisp: %usb-init, then interface additions
;;;; (%usb-hid-add, %usb-serial-add, %usb-mouse-add), then %usb-start to
;;;; freeze and enumerate. The first CDC console (print/format) is always
;;;; exposed. Device identity is settable before start via %usb-vid /
;;;; %usb-pid / %usb-manufacturer / %usb-product / %usb-serial.
;;;;
;;;; Here we add just the HID keyboard. A button wired active-low to GND
;;;; (internal pull-up) sends the HID key 'a'. The button pin is selected per
;;;; board with reader conditionals (see below).
;;;;
;;;; NOTE: this program requires %hid-key, which only exists on targets whose
;;;; USB controller supports the HID device class (see the target's .machine
;;;; metadata "usb" section). It will not compile on e.g. the ESP32-C3, whose
;;;; USB is a fixed-function Serial/JTAG port (no HID).
;;;;
;;;; Because main lives in the USB-KEYBOARD package (not SECD), point the
;;;; linker at it explicitly:
;;;;   (secd-lisp:secd-compile-file "examples/usb-keyboard.lisp"
;;;;                                :target :rp2040-pico
;;;;                                :entry "USB-KEYBOARD:MAIN")
;;;;
;;;; nRF52840 (nice!nano / ProMicro clone):
;;;;   (secd-lisp:secd-compile-file "examples/usb-keyboard.lisp"
;;;;                                :target :nrf52840-promicro
;;;;                                :entry "USB-KEYBOARD:MAIN")

(defpackage :usb-keyboard)

(defconstant +usage-a+ 4)      ; HID keyboard usage code for 'a'

;;; Button pin (active-low, wired to GND, internal pull-up).
;;;   nRF52840 ProMicro clone: P0.02  (P0.31 is held low on most nice!nano
;;;                                clones — RGB-LED data / pull-down — so it
;;;                                can't be used as an input there)
;;;   RP2040-family boards:     GPIO 10 (original wiring)
#+nrf52840-promicro (defconstant +button-pin+ 2)
#-nrf52840-promicro (defconstant +button-pin+ 10)

(defun button-pressed ()
  ; Active-low: the pin has an internal pull-up, so it reads 1 when the
  ; button is open and 0 when the pin is connected to GND.
  (= (%gpio-read +button-pin+) 0))

(defun main ()
  ; Lisp builds the USB interface set before enabling. Only the standard
  ; console (print/format) plus the HID keyboard; no extra user serial ports.
  (%usb-init)
  (%usb-hid-add)
  (%usb-start)                             ; freeze + enumerate
  (%gpio-init +button-pin+ :input)
  (loop
    (when (button-pressed)
      ; 0 = no modifiers, +usage-a+ = 'a'
      (%hid-key 0 +usage-a+))
    (%sleep 200)))