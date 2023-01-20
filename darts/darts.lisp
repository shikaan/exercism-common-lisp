(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun score (x y)
  (let ((r (sqrt (+ (* x x) (* y y))))
    (cond
     ((and (> r 5) (< r 10)) 1)
     ((and (> r 1) (<= r 5)) 5)
     ((< r 1) 10)
     (t 0)))))
