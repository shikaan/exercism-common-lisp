(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
  "Transforms hash values into keys with their keys as their values."
  (let ((result (make-hash-table)))
    (maphash 
      #'(lambda (k v) 
        (loop for c in v do (setf (gethash (char-downcase c) result) k))) 
      data)
    result))
