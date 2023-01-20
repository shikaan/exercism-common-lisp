(defpackage :queen-attack
  (:use :cl)
  (:export :valid-position-p
           :attackp))

(in-package :queen-attack)

(defun c-row (coordinates) (car coordinates))
(defun c-col (coordinates) (cdr coordinates))

(defun valid-position-p (coordinates) 
  (and
    (< 0 (c-row coordinates))
    (> 8 (c-row coordinates))
    (< 0 (c-col coordinates))
    (> 8 (c-col coordinates))))

(defun dist (a b) (abs (- a b)))

(defun attackp (white-queen black-queen)
  (or 
    (= (c-row white-queen) (c-row black-queen))
    (= (c-col white-queen) (c-col black-queen))
    (= (dist (c-col white-queen) (c-col black-queen)) (dist (c-row white-queen) (c-row black-queen)))))
