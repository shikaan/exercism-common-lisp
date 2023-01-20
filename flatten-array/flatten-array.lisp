(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun flatten (nested)
  (if (listp nested)
    (reduce #'append nested :key #'flatten)
    (list nested)))
