(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun multiple-p (i) (lambda (x) (zerop (mod i x))))

(defun sum (factors limit)
  (loop for i from 1 below limit if (some (multiple-p i) (remove 0 factors)) sum i))
