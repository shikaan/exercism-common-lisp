(defpackage :pangram
  (:use :cl)
  (:export :pangramp))

(in-package :pangram)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun pangramp (sentence)
  (setf sentence (string-downcase sentence))
  (loop for c across *alphabet* always (position c sentence)))
