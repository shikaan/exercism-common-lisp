(defpackage :allergies
  (:use :cl)
  (:shadow :list)
  (:export :allergic-to-p :list :allergens))

(in-package :allergies)

(defvar allergens '("cats" "pollen" "chocolate" "tomatoes" "strawberries" "shellfish" "peanuts" "eggs"))

(defun value-to-index (value) (floor (log value 2)))
(defun score-to-bitstring (score) (format nil "~8,'0b" score))

(defun allergic-to-p (score allergen)
  "Returns true if given allergy score includes given allergen."
  (format nil "~a" (score-to-bitstring score))
  (char= (char (score-to-bitstring score) (position allergen allergens :test #'string=)) #\1))

(defun list (score)
  "Returns a list of allergens for a given allergy score."
  (loop 
      for bit across (reverse (score-to-bitstring score)) 
      for i from 7 downto 0
      if (char= bit #\1) collect (nth i allergens)))
