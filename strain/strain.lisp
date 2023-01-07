(defpackage :strain
  (:use :cl)
  (:export :keep :discard))

(in-package :strain)

(defun keep (keep-p elements)
  "Returns a sublist of elements according to a given predicate."
  (remove-if-not keep-p elements))

(defun discard (discard-p elements)
  "Returns a sublist of elements not matching a given predicate."
  (remove-if discard-p elements))
