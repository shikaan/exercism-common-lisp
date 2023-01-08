(defpackage :robot-name
  (:use :cl)
  (:export :build-robot :robot-name :reset-name))

(in-package :robot-name)

(defstruct robot name)

(defun random-char () (code-char (+ 65 (random 26))))

(defun generate-name () (format nil "~a~a~a" (random-char) (random-char) (random 999)))

(defun build-robot () (make-robot :name (generate-name)))

(defun reset-name (robot)
  (setf (robot-name robot) (generate-name)))