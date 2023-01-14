(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplet-p (a b c)
  (and
   (< a b c)
   (= (expt c 2) (+ (expt a 2) (expt b 2)))))

(defun triples-with-sum-in-range (left right index sum)
  (loop while (< left right) 
    do (cond 
      ((and (= sum (+ index left right)) (triplet-p index left right)) 
        (return (list index left right)))
      ; Advance left
      ((> sum (+ index left right)) (setf left (1+ left)))
      ; Move back right
      (t (setf right (1- right))))))

(defun triplets-with-sum (n)
  (loop 
      for i from 1 to n
      for result = (triples-with-sum-in-range (1+ i) (1- n) i n)
      when result collect result))
