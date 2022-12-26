(defpackage :gigasecond-anniversary
  (:use :cl)
  (:export :from))
(in-package :gigasecond-anniversary)

(defconstant ANNIVERSARY 1000000000)



(defun from (year month day hour minute second)
  (multiple-value-bind (s mi h d mo y)
    (decode-universal-time
      (+
        (encode-universal-time second minute hour day month year 0)
        anniversary) 0)
    (list y mo d h mi s)))