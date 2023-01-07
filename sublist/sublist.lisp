(defpackage :sublist
  (:use :cl)
  (:export :sublist))

(in-package :sublist)

(defun sublist (list1 list2)
  "what is list1 of list2 (sublist, superlist, equal or unequal)"
  (cond 
    ((equalp list1 list2) :equal)
    ((search list1 list2 :test #'equalp) :sublist)
    ((search list2 list1 :test #'equalp) :superlist)
    (t :unequal)))
