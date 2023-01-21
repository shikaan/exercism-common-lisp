(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun palindrome-p (n)
  (let ((s (format nil "~a" n)))
    (string= s (reverse s))))

(defun smallest (min-factor max-factor)
  (if (< min-factor max-factor)
    (loop 
      with minimum = nil
      with minimum-couple = '() 
      for a from min-factor to max-factor
        do (loop for b from a to max-factor
            for p = (* a b)
            if (palindrome-p p)
            do (cond
              ((or (null minimum) (< p minimum)) (progn (setf minimum p) (setf minimum-couple (list `(,a ,b)))))
              ((= p minimum) (setf minimum-couple (append minimum-couple (list `(,a ,b)))))))
        finally (return (values minimum minimum-couple)))))

(defun largest (min-factor max-factor)
  (if (< min-factor max-factor)
    (loop 
      with maximum = nil
      with maximum-couple = '() 
      for a from min-factor to max-factor
        do (loop for b from a to max-factor
            for p = (* a b) 
            if (palindrome-p p)
            do (cond
              ((or (null maximum) (> p maximum)) (progn (setf maximum p) (setf maximum-couple (list `(,a ,b)))))
              ((= p maximum) (setf maximum-couple (append maximum-couple (list `(,a ,b)))))))
        finally (return (values maximum maximum-couple)))))
