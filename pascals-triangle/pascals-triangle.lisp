(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))
(in-package :pascals-triangle)

(defun row (n)
  (case n
      (1 '(1))
      (2 '(1 1))
      (otherwise
        (append 
          (loop for el in (row (1- n))
              and prev-el = 0 then el
              collect (+ el prev-el))
          '(1)))))

(defun rows (n)
  (loop for i from 1 to n collect (row i)))
