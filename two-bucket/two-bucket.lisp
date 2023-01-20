(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)

(defun solvable-p (bucket-one bucket-two goal) 
  (zerop (mod goal (gcd bucket-one bucket-two))))

(defun pour (from from-sym to to-sym goal)
  (loop for move from 1
      with content-from = from
      with content-to = 0
      with quantity
      do (cond
        ((= content-from goal) (return (list `(:moves . ,move) `(:goal-bucket . ,from-sym) `(:other-bucket . ,content-to))))
        ((= content-to goal) (return (list `(:moves . ,move) `(:goal-bucket . ,to-sym) `(:other-bucket . ,content-from))))
        ((= content-from 0) (setf content-from from))
        ((= content-to to) (setf content-to 0))
        (t
          (setf quantity (min content-from (- to content-to)))
          (setf content-to (+ content-to quantity))
          (setf content-from (- content-from quantity))))))

(defun measure (bucket-one bucket-two goal start-bucket)
  "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an alist of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
  (if (solvable-p bucket-one bucket-two goal)
    (if (eql start-bucket :one)
      (pour bucket-one :one bucket-two :two goal)
      (pour bucket-two :two bucket-one :one goal))))
  