(defpackage :scrabble-score
  (:use :cl)
  (:export :score-word))

(in-package :scrabble-score)

(defun score-letter (c)
  (setf c (char-downcase c))
  (cond 
    ((position c "aeioulnrst") 1)
    ((position c "dg") 2)
    ((position c "bcmp") 3)
    ((position c "fhvwy") 4)
    ((position c "k") 5)
    ((position c "jx") 8)
    ((position c "qz") 10)
    (t 0)
  ))

(defun score-word (word)
  "Computes the score for an entire word."
  (loop for c across word sum (score-letter c)))
