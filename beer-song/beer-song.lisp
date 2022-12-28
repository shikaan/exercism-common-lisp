(defpackage :beer-song
  (:use :cl)
  (:export :verse :sing))
(in-package :beer-song)

(defun bottles-of-beer (n)
  "Returns a string representing the number of bottles of beer"
  (cond 
    ((= 0 n) "no more bottles of beer")
    (t (format nil "~D bottle~:P of beer" n))))

(defun response (n)
  "Returns the response sentence"
  (cond 
    ((= n 0) "Go to the store and buy some more")
    ((= n 1) "Take it down and pass it around")
    (t "Take one down and pass it around")))

(defun verse (n)
  "Returns a string verse for a given number."
  (format
    nil
    "~@(~a~) on the wall, ~a.~&~
     ~a, ~a on the wall.~%"
    (bottles-of-beer n) 
    (bottles-of-beer n)
    (response n) 
    (bottles-of-beer (mod (1- n) 100))))

(defun sing (start &optional (end 0))
  "Returns a string of verses for a given range of numbers."
  (format nil "~{~A~%~}" (loop for i from start downto end collect (verse i))))
