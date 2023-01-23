(defpackage :proverb
  (:use :cl)
  (:export :recite))

(in-package :proverb)

(defun pairs (l)
  (loop for i from 0 below (1- (length l)) 
    collect (list (nth i l) (nth (1+ i) l))))

(defun recite (strings)
  (if strings
    (format nil
      "~{~A~^~%~}" 
      (append 
        (loop for p in (pairs strings)
          collect (format nil "For want of a ~A the ~A was lost." (first p) (second p)))
        (list (format nil "And all for the want of a ~A." (first strings)))))
    ""))
