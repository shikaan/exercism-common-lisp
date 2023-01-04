(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))
(in-package :pascals-triangle)

(defun row (n)
  (case n
      (1 '(1))
      (otherwise
        (append 
          (loop for n in (row (1- n))
              and n-1 = 0 then n
              collect (+ n n-1))
          '(1)))))

(defun rows (n)
  (loop for i from 1 to n collect (row i)))
