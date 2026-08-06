;;;; ws2812.lisp — WS2812 RGB LED driver library (pure secd-lisp)
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Drives a WS2812 (NeoPixel) LED using the `%wave-play` primitive.
;;;; Durations are in 100ns ticks; %wave-play flips the pin level after each
;;;; one, so a frame is just the alternating high/low pulse durations.
;;;;
;;;;   0-bit: high 350ns, low 800ns  -> ticks (3 8)
;;;;   1-bit: high 700ns, low 600ns  -> ticks (7 6)
;;;;
;;;; A pixel colour is a list (r g b); the three bytes are sent in that
;;; order, so (255 0 0) lights the red channel of the first LED.
;;;;
;;;; Functions are exported as ws2812:rgb-show and ws2812:rgb-off.

(defpackage "WS2812"
  (:export RGB-SHOW RGB-OFF))

;; High/low duration pairs for an 8-bit byte, transmitted MSB-first (as
;; WS2812 requires). Bits are extracted LSB-first but cons'd onto CELLS, so
;; the highest bit ends up at the front of the returned list.
(defun byte-cells (v i cells)
  (if (< i 8)
      (if (= (mod v 2) 1)
          (byte-cells (/ v 2) (+ i 1) (cons 7 (cons 6 cells)))
          (byte-cells (/ v 2) (+ i 1) (cons 3 (cons 8 cells))))
      cells))

;; Concatenate two lists (append).
(defun concat (a b)
  (if (null? a) b (cons (car a) (concat (cdr a) b))))

;; Durations for one pixel, in the listed (r g b) order.
(defun pixel-cells (px)
  (concat (byte-cells (car px) 0 nil)
          (concat (byte-cells (car (cdr px)) 0 nil)
                  (byte-cells (car (cdr (cdr px))) 0 nil))))

;; Durations for a whole strip of pixels.
(defun strip-cells (strip)
  (if (null? strip)
      nil
      (concat (pixel-cells (car strip))
              (strip-cells (cdr strip)))))

;; Show one frame: send every pixel (RGB), then hold the line low for a
;; reset pulse (>50us; here 1ms) so the strip latches the frame. The pixel
;; keeps this colour until the next frame; the caller controls how long it
;; stays lit by sleeping after this call.
(defun rgb-show (pin strip)
  (progn
    (%wave-play pin 1 (strip-cells strip))
    (%gpio-write pin 0)
    (%sleep 1)))

;; Build a strip of COUNT identical black pixels.
(defun black-strip (count)
  (if (= count 0)
      nil
      (cons (list 0 0 0) (black-strip (- count 1)))))

;; Turn off COUNT leading pixels by transmitting COUNT black (0 0 0) pixels.
;; For a single on-board LED pass 1; for an N-pixel strip pass N to blank the
;; whole strip.
(defun rgb-off (pin count)
  (rgb-show pin (black-strip count)))