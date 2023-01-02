(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun to-decimal (list-digits in-base)
  "Converts a number given as a list of digits and a base to decimal"
  (if (zerop (length list-digits)) 0
    (loop 
      for digit in (reverse list-digits)
      for i from 0 
      sum (* digit (expt in-base i)))))

(defun div (num den) 
  "Performs a division with remainder"
  (values (floor (/ num den)) (mod num den)))

(defun convert-decimal-to (n base &optional (acc '()))
  "Converts a decimal to a given base"
  (if (zerop n) 
    (if (zerop (length acc)) '(0) (reverse acc))
    (multiple-value-bind (q r) (div n base) 
      (convert-decimal-to q base (append acc (list r))))))

(defun valid-p (digits base) 
  (every #'(lambda (x) (and (<= 0 x) (< x base)))  digits))

(defun rebase (list-digits in-base out-base)
  (cond
    ((< in-base 2) nil)
    ((< out-base 2) nil)
    ((null list-digits) '(0))
    ((not (valid-p list-digits in-base)) nil)
    (t (convert-decimal-to (to-decimal list-digits in-base) out-base))
  ))