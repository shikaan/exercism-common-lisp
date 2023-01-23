(defpackage :spiral-matrix
  (:use :cl)
  (:export :spiral-matrix))

(in-package :spiral-matrix)

(defun p-sum (a b) (mapcar #'+ a b))
(defun p-row (a) (first a))
(defun p-col (a) (second a))

(defun rotate (p) 
  (cond
    ((equal p '(0 1)) '(1 0))
    ((equal p '(1 0)) '(0 -1))
    ((equal p '(0 -1)) '(-1 0))
    ((equal p '(-1 0)) '(0 1))))

(defun spiral-matrix (size)
  (when (> size 0)
    (loop for n from 1 to (* size size)
          with cur = '(0 0)
          with nex = '(0 1)
          with dir = '(0 1)
          with m = (make-array `(,size ,size) :initial-element nil)
      do (progn 
          (setf (aref m (p-row cur) (p-col cur)) n)
          (setf nex (p-sum cur dir))
          (if (and (array-in-bounds-p m (p-row nex) (p-col nex)) (null (aref m (p-row nex) (p-col nex)))) 
            (setf cur nex)
            (progn
              (setf dir (rotate dir))
              (setf cur (p-sum cur dir)))))
      finally (return m))))
