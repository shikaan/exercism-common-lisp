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
    (matched (remove-if-not (equal-n-p fst) (remove-duplicates (rest l)))))
    (setf matched (push fst matched))
    (if (>= (length matched) n)
      (let* (
        (sublist (subseq matched 0 n))
        (remainder (remove-many l sublist)))
        (groups-of remainder n (push sublist accumulator)))
      (list accumulator l))))

; In order to optimise groups we need to replace the (5,3) couples
; with (4,4) couples.
(defun opt (bins)
  (loop while (and (> (first bins) 0) (> (third bins) 0))
    do (setf bins 
        (list 
          (1- (first bins))
          (+ 2 (second bins))
          (1- (third bins))
          (fourth bins)
          (fifth bins))))
  bins)

(defun calculate-price (basket)
  (let* (
    (five (groups-of basket 5))
    (four (groups-of (second five) 4))
    (three (groups-of (second four) 3))
    (two (groups-of (second three) 2))
    (bins (list (length (first five)) (length (first four)) (length (first three)) (length (first two)) (length (second two)))))
    (setf bins (opt bins))
    (+
      (* 3000 (first bins))
      (* 2560 (second bins))
      (* 2160 (third bins))
      (* 1520 (fourth bins))
      (* 800 (fifth bins)))))
