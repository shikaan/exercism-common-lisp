(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

(defun item-value (item) (cdr (second item)))
(defun item-weight (item) (cdr (first item)))
(defun empty-p (seq) (zerop (length seq)))
(defun last-el (seq) (first (last seq)))

(defun maximum-value (maximum-weight items)
  (let* ((l (last-el items)) (value (item-value l)) (weight (item-weight l)))
  (cond 
    ((or (empty-p items) (zerop maximum-weight)) 0)
    ((> weight maximum-weight) (maximum-value maximum-weight (butlast items)))
    (t (max 
      (+ value (maximum-value (- maximum-weight weight) (butlast items))) 
      (maximum-value maximum-weight (butlast items)))))))