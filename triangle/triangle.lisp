(defpackage :triangle
  (:use :cl)
  (:export :triangle-type-p))

(in-package :triangle)

(defun positive-p (n) (> n 0))

(defun triangle-p (a b c)
  (and
    (positive-p a) (positive-p b) (positive-p c)
    (<= c (+ a b)) (<= b (+ a c)) (<= a (+ c b))))

(defun triangle-type-p (type a b c)
  "Deterimines if a triangle (given by side lengths A, B, C) is of the given TYPE"
  (when (triangle-p a b c)
    (case type
      (:equilateral (= a b c))
      (:isosceles (or (= a b) (= b c) (= a c)))
      (:scalene (and (/= a b) (/= b c) (/= a c))))))
