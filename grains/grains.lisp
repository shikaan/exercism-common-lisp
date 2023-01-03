(defpackage :grains
  (:use :cl)
  (:export :square :total))
(in-package :grains)

(defun square (n) (expt 2 (1- n)))

(defun total () (loop for i from 1 to 64 sum (square i)))
