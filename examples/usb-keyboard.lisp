;;;;; usb-keyboard.lisp — USB device exposed from Lisp (CherryUSB).
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Runs on a HID-capable board (e.g. RP2040 / RP2350). USB composite is
;;;; brought up from Lisp via %usb-init with a mask that selects which
;;;; interfaces the host sees:
;;;;   bit0 = HID keyboard, bit1 = Lisp-owned serial console.
;;;; The first CDC console (print/format) is always exposed.
;;;;
;;;; Here we ask for BOTH: (%usb-init 3). A button on GPIO 10 sends the
;;;; HID key 'a'; every byte received on the Lisp serial console is echoed
;;;; back to it.
;;;;
;;;; NOTE: this program requires %hid-key, which only exists on targets whose
;;;; USB controller supports the HID device class (see the target's .machine
;;;; metadata "usb" section). It will not compile on e.g. the ESP32-C3, whose
;;;; USB is a fixed-function Serial/JTAG port (no HID).

(defpackage :usb-keyboard)

(defconstant +button-pin+ 10)  ; GPIO 10 (button to GND, internal pull-up)
(defconstant +usage-a+ 4)      ; HID keyboard usage code for 'a'

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