(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))
(in-package :leap)

(defun divisible-p (n f) (zerop (mod n f)))

(defun leap-year-p (year)
  (and
    (divisible-p year 4) 
    (or
      (and (divisible-p year 100) (divisible-p year 400))
      (not (divisible-p year 100)))))
