(defpackage :say
  (:use :cl)
  (:export :say))

(in-package :say)

(defun say (number) 
  (unless (or (< number 0) (> number 999999999999))
    (format nil "~R" number)))
