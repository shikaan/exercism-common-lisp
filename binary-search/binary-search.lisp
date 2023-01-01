(defpackage :binary-search
  (:use :cl)
  (:export :binary-find :value-error))

(in-package :binary-search)

(defun binary-find (arr el &optional (offset 0))
  (case (length arr)
    (0 nil)
    (1 (if (equal (aref arr 0) el) offset))
    (otherwise 
      (let ((m (floor (/ (length arr) 2))))
      (cond 
          ((= (aref arr m) el) (+ offset m))
          ((< (aref arr m) el) (binary-find (subseq arr m) el (+ offset m)))
          ((> (aref arr m) el) (binary-find (subseq arr 0 m) el offset)))))))
