(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun rebase (list-digits in-base out-base)
  (convert-decimal-to (to-decimal list-digits in-base) out-base))

(defun to-decimal (list-digits in-base)
  "Converts a number given as a list of digits and a base to decimal"
  (loop 
    for digit in (reverse list-digits)
    for i from 0 
    sum (* digit (expt in-base i))))

(defun div (num den) 
  "Performs a division with remainder"
  (values (floor (/ num den)) (mod num den)))

(defun convert-decimal-to (n base &optional (acc '()))
  "Converts a decimal to a given base"
  (if (zerop n) 
    (reverse acc)
    (multiple-value-bind (q r) (div n base) 
      (convert-decimal-to q base (append acc (list r))))))