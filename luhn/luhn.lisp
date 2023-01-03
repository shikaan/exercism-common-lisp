(defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)

(defun double (n)
  "Double number according to Luhn Algorithm"
  (let ((d (* 2 n)))
    (if (> d 9) (- d 9) d)))

(defun clean (s) 
  "Remove spaces from string"
  (remove #\Space s))

(defun int (c) 
  "Convert character to integer"
  (parse-integer (string c)))

(defun luhn (s)
  "Returns the Luhn number for a valid, clean string"
  (loop for i from 1 for d across (reverse s) 
    sum (if (zerop (mod i 2)) (double (int d)) (int d))))

(defun validp (input)
  (let ((c (clean input)))
    (cond 
      ((<= (length c) 1) nil)
      ((notevery #'digit-char-p c) nil)
      (t (= 0 (mod (luhn c) 10))))))
