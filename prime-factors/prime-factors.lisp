(defpackage :prime-factors
  (:use :cl)
  (:export :factors))

(in-package :prime-factors)

(defun divisible-p (n d) (zerop (mod n d)))
(defun lpush (l item) (append l (list item)))

(defun factors (n &optional acc)
  (cond
    ((= n 1) acc)
    ((evenp n) (factors (/ n 2) (lpush acc 2)))
    (t (loop for i from 3 to n
      if (divisible-p n i) 
      do (return (factors (/ n i) (lpush acc i)))))))
