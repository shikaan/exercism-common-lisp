(defpackage :robot-simulator
  (:use :cl)
  (:export :+north+ :+east+ :+south+ :+west+ :execute-sequence
           :robot :robot-position :robot-bearing :make-robot))

(in-package :robot-simulator)

(defvar +north+ '(0 . 1))
(defvar +east+ '(1 . 0))
(defvar +south+ '(0 . -1))
(defvar +west+ '(-1 . 0))

(defvar +bearings+ (list +north+ +east+ +south+ +west+))

(defun next-bearing (current)
  (nth (mod (1+ (position current +bearings+)) 4) +bearings+))

(defun prev-bearing (current)
  (nth (mod (1- (position current +bearings+)) 4) +bearings+))

(defstruct robot 
  (position '(0 . 0)) 
  (bearing +north+))

(defun p+ (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun move (robot delta)
  (setf (robot-position robot) (p+ delta (robot-position robot))))

(defun left (robot)
  (setf (robot-bearing robot) (prev-bearing (robot-bearing robot))))

(defun right (robot)
  (setf (robot-bearing robot) (next-bearing (robot-bearing robot))))

(defun advance (robot)
  (move robot (robot-bearing robot)))

(defun execute-sequence (robot sequence)
  (loop for c across sequence
    do (case c
      (#\L (left robot))
      (#\R (right robot))
      (#\A (advance robot)))))