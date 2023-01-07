(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)

(defun aliquot-sum (n)
  (loop 
    for i from 1 to (1- n)
    if (zerop (mod n i))
    sum i into result 
    finally (return (or result 0))))

(defun classify (n)
  (if (> n 0)
    (let ((s (aliquot-sum n)))
      (cond
        ((= s n) "perfect")
        ((> s n) "abundant")
        ((< s n) "deficient")
      ))))
