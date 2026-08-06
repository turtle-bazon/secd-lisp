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
;;;; A pixel colour is a list (r g b); WS2812 wants it in GRB order.
;;;;
;;;; Functions are exported as ws2812:rgb-show and ws2812:rgb-off.

(defpackage "WS2812"
  (:export RGB-SHOW RGB-OFF))

;; MSB-first high/low duration pairs for an 8-bit byte. Bits are extracted
;; LSB-first (via mod//) but cons'd so the result ends up MSB-first.
(defun byte-cells (v i)
  (if (< i 8)
      (if (= (mod v 2) 1)
          (cons 7 (cons 6 (byte-cells (/ v 2) (+ i 1))))
          (cons 3 (cons 8 (byte-cells (/ v 2) (+ i 1)))))
      nil))

;; Concatenate two lists (append).
(defun concat (a b)
  (if (null? a) b (cons (car a) (concat (cdr a) b))))

;; Durations for one pixel, in GRB order.
(defun pixel-cells (px)
  (concat (byte-cells (car (cdr px)) 0)
          (concat (byte-cells (car px) 0)
                  (byte-cells (car (cdr (cdr px))) 0))))

;; Durations for a whole strip of pixels.
(defun strip-cells (strip)
  (if (null? strip)
      nil
      (concat (pixel-cells (car strip))
              (strip-cells (cdr strip)))))

;; Show one frame: send every pixel (GRB), then hold the line low for a
;; reset pulse (>50us; here 1ms) so the strip latches the frame. The pixel
;; keeps this colour until the next frame; the caller controls how long it
;; stays lit by sleeping after this call.
(defun rgb-show (pin strip)
  (progn
    (%wave-play pin 1 (strip-cells strip))
    (%gpio-write pin 0)
    (%sleep 1)))

;; Turn all pixels off by transmitting a black (0 0 0) frame.
(defun rgb-off (pin)
  (rgb-show pin (list (list 0 0 0))))