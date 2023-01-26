(defpackage :book-store
  (:use :cl)
  (:export :calculate-price))

(in-package :book-store)

(defun equal-n-p (f) (lambda (x) (/= x f)))

(defun remove-many (a b)
  (loop for el in b do (setf a (remove el a :count 1)))
  a)

(defun groups-of (l n &optional accumulator)
  (let* (
    (fst (first l))
    (matched (remove-if-not (equal-n-p fst) (rest l))))
    (setf matched (push fst matched))
    (if (>= (length matched) n)
      (let* (
        (sublist (subseq matched 0 n))
        (remainder (remove-many l sublist)))
        (groups-of remainder n (push sublist accumulator)))
      (list accumulator l))))

(defun price (l)
  (cond ((not (listp l)) 800)
        ((= (length l) 5) 3000)
        ((= (length l) 4) 2560)
        ((= (length l) 3) 2160)
        ((= (length l) 2) 1520)
        (t 800)))

(defun calculate-price (basket)
  (let* (
    (five (groups-of basket 5))
    (four (groups-of (second five) 4))
    (three (groups-of (second four) 3))
    (two (groups-of (second three) 2)))  
    (reduce #'+ (append (first five) (first four) (first three) (first two) (second two)) :key #'price)))
