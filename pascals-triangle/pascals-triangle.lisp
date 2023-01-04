(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))
(in-package :pascals-triangle)

(defun row (n)
  (if (= 1 n) '(1)
    (loop for n in (append (row (1- n)) '(0))
        and n-1 = 0 then n
        collect (+ n n-1))))

(defun rows (n)
  (loop for i from 1 to n collect (row i)))
