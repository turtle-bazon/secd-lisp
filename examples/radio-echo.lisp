;;;;; radio-echo.lisp — generic 2.4GHz radio loop (nRF52840 ESB / ESP32 ESP-NOW).
;;;;
;;;;  Copyright (C) 2026 — License: GPL3
;;;;
;;;;  Two boards loaded with the SAME shared address exchange packets. Each
;;;;  board inits the radio, adopts the shared address + channel, then in a
;;;;  loop sends a fixed probe packet and blinks its LED whenever it receives
;;;;  one. The same bytecode runs on any target that implements %radio-*
;;;;  (nRF52840 via ESB, ESP32 via ESP-NOW); on targets whose %radio-* are
;;;;  stubs the calls simply return -1 / nil.
;;;;
;;;;  The address byte-vector is interpreted per-platform:
;;;;    - nRF52840 ESB : last 5 bytes (prefix + 4-byte base)
;;;;    - ESP32 ESP-NOW: all 6 bytes as the peer MAC
;;;;  Supplying 6 bytes works for both.
;;;;
;;;;  Compile / flash:
;;;;   (secd-lisp:secd-compile-file "examples/radio-echo.lisp"
;;;;                                :target :nrf52840-promicro
;;;;                                :entry "RADIO-ECHO:MAIN")

(defpackage :radio-echo)

(defconstant +addr+ #(0xE7 0xE7 0xE7 0xE7 0xE7 0xE7))
(defconstant +channel+ 2)

;;; LED pin is board-specific; override per target with reader conditionals.
;;; V1940 ProMicro nRF52840: nice!nano blue LED on P0.15.
#+nrf52840-promicro (defconstant +led+ 15)
#-nrf52840-promicro (defconstant +led+ 2)
(defconstant +led-on+ 1)

(defun main ()
  (%radio-init)
  (%radio-address +addr+)
  (%radio-set-channel +channel+)
  (%gpio-init +led+ :output)
  (loop
    (%radio-send #(0xDE 0xAD 0xBE 0xEF))
    (let ((pkt (%radio-recv)))
      (when pkt
        (%gpio-write +led+ +led-on+)
        (%sleep 50)
        (%gpio-write +led+ 0)))
    (%sleep 500)))
