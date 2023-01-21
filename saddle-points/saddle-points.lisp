(defpackage :saddle-points
  (:use :cl)
  (:export :saddle-points))

(in-package :saddle-points)

(defun matrix-row (matrix row) 
  (loop for i from 0 below (array-dimension matrix 1) 
    collect (aref matrix row i)))

(defun matrix-col (matrix col) 
  (loop for i from 0 below (array-dimension matrix 0) 
    collect (aref matrix i col)))

(defun saddle-p (matrix row col)
  (decf row) (decf col)
  (let ((v (aref matrix row col)))
    (and
      (every #'(lambda (x) (>= v x)) (matrix-row matrix row))
      (every #'(lambda (x) (<= v x)) (matrix-col matrix col)))))

(defun saddle-points (matrix)
  (loop
    with result
    for row from 1 to (array-dimension matrix 0)
      do (loop for col from 1 to (array-dimension matrix 1) 
        if (saddle-p matrix row col)
          do (push (list row col) result))
      finally (return (reverse result))))