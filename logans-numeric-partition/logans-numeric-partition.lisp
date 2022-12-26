(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

;; Define categorize-number function
(defun categorize-number (lists number) 
  (if (oddp number) 
    (cons (append (list number) (first lists)) (rest lists))
    (cons (first lists) (append (list number) (rest lists)))))

;; Define partition-numbers function
(defun partition-numbers (l) 
  (reduce #'categorize-number l :initial-value (cons '() '())))