(defpackage :nth-prime
  (:use :cl)
  (:export :find-prime))

(in-package :nth-prime)

(defun prime-p (n)
  (or
    (= n 2)
    (= n 3)
    (unless (or (evenp n) (zerop (mod n 3)))
      (loop for i from 5 to (ceiling (sqrt n))
        never (zerop (mod n i))))))

(defun find-prime (number)
  (unless (zerop number)
    (loop for i from 2 with c = 0
      if (prime-p i)
        do (if (= c (1- number)) 
          (return i)
          (incf c)))))
