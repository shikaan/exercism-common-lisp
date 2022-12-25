(defpackage :log-levels
  (:use :cl)
  (:export :log-message :log-severity :log-format))

(in-package :log-levels)


(defun log-message (log-string) (subseq log-string 8))

(defun log-severity (log-string)
  (let ((severity (subseq log-string 1 5)))
  (cond
    ((string-equal severity 'info) :everything-ok)
    ((string-equal severity 'warn) :getting-worried)
    ((string-equal severity 'ohno) :run-for-cover))))

(defun log-format (log-string)
  (case (log-severity log-string)
    (:everything-ok (string-downcase (log-message log-string)))
    (:getting-worried (string-capitalize (log-message log-string)))
    (:run-for-cover (string-upcase (log-message log-string)))
  ))
