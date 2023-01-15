(defpackage :diamond
  (:use :cl)
  (:export :rows))

(in-package :diamond)

(defvar *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun diamond-size (input-idx) 
  "Returns the size of the diamond given the index of the input letter"
  (1+ (* 2 input-idx)))

(defun insert-at (index char string)
  "Insert charachter at given index in the given string"
  (coerce (loop 
    for c across string 
    for i from 0
    collect (if (= index i) char c)) 'string))

(defun make-line (n) 
  "Returns an empty string of length n"
  (coerce (loop for i from 1 to n collect #\Space) 'string))

(defun up-to-and-down-to (n) 
  "Return a list (0...n...0)"
  (append (loop for i from 0 to n collect i) (loop for i from (1- n) downto 0 collect i)))

(defun rows (letter)
  (let* ((idx (position letter *alphabet*)) (size (diamond-size idx)) (half-size (floor (/ size 2))))
    (loop 
      for row in (up-to-and-down-to idx)
      for l-idx = (mod row (1+ idx))
      for l = (char *alphabet* l-idx)
      collect (insert-at (- half-size l-idx) l (insert-at (+ half-size l-idx) l (make-line size))))))
