(defpackage :roman-numerals
  (:use :cl)
  (:export :romanize))

(in-package :roman-numerals)

(defun romanize (number)
  "Returns the Roman numeral representation for a given number."
  (format nil "~{~A~}"
    (reverse 
      (loop 
        for num in (reverse (to-num-list number)) 
        for j from 0
        collect (convert num (symbols j))))))

(defun symbols (pos)
  "Retrieves the symbols needed for parsing the digit, from right position (e.g., units are in position 0)"
  (case pos
    (0 (list "I" "V" "X"))
    (1 (list "X" "L" "C"))
    (2 (list "C" "D" "M"))
    (3 (list "M"))))

(defun convert (num symbols)
  (let (
    (fst (first symbols))
    (snd (second symbols))
    (trd (third symbols)))
    (case num
      (1 fst)
      (2 (concatenate 'string fst fst))
      (3 (concatenate 'string fst fst fst))
      (4 (concatenate 'string fst snd))
      (5 snd)
      (6 (concatenate 'string snd fst))
      (7 (concatenate 'string snd fst fst))
      (8 (concatenate 'string snd fst fst fst))
      (9 (concatenate 'string fst trd))
      (otherwise ""))))

(defun to-num-list (num)
  "Converts a number to list of numbers"
  (map 
    'list 
    #'(lambda (x) (parse-integer (string x)))
    (format nil "~a" num)))