(defpackage :reporting-for-duty
  (:use :cl)
  (:export :format-quarter-value :format-two-quarters
           :format-two-quarters-for-reading))

(in-package :reporting-for-duty)

;; Define format-quarter-value function.
(defun format-quarter-value (q v) (format nil "The value ~a quarter: ~a" q v))

;; Define format-two-quarters function.
(defun format-two-quarters (stream q1 v1 q2 v2)
  (format stream "~%~a~%~a~%" (format-quarter-value q1 v1) (format-quarter-value q2 v2)))

;; Define format-two-quarters-for-reading function.
(defun format-two-quarters-for-reading (stream q1 v1 q2 v2)
  (format stream "(~s ~s)" (format-quarter-value q1 v1) (format-quarter-value q2 v2)))
