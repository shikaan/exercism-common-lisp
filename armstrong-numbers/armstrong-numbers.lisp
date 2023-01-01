(defpackage :armstrong-numbers
  (:use :cl)
  (:export :armstrong-number-p))
(in-package :armstrong-numbers)

(defun number-list (number)
  (map 
    'list 
    #'(lambda (x) (parse-integer (string x))) 
    (format nil "~a" number)))

(defun armstrong-number-p (number)
  (let ((nrs (number-list number)))
    (= 
      number 
      (reduce 
        #'(lambda (acc n) (+ acc (expt n (length nrs)))) 
        nrs 
        :initial-value 0))))
