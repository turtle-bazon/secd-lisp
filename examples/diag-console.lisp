(defpackage :diag (:use :common-lisp :secd-lisp) (:export :main))
(in-package :diag)

(defun main ()
  (%usb-init)
  (%usb-hid-add)
  (%usb-mouse-add)
  (%usb-start)
  (let ((tick 0))
    (loop
      (print tick)
      (%hid-mouse 0 1 0 0)
      (%sleep 100)
      (setf tick (+ tick 1)))))