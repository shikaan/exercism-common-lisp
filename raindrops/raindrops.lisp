(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun divisible-by-p (n m) (zerop (mod n m)))

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((result ""))
    (if (divisible-by-p n 3) (setf result "Pling"))
    (if (divisible-by-p n 5) (setf result (format nil "~APlang" result)))
    (if (divisible-by-p n 7) (setf result (format nil "~APlong" result)))
    (if (zerop (length result)) (setf result (format nil "~A" n)))
    result))
