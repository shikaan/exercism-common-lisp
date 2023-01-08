(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))
(in-package :crypto-square)

(defun get-size (str)
  (let ((size (length str)))
    (let ((sq (round (sqrt size))))
      (cond 
        ((>= (* (1- sq) sq) size) (list (1- sq) sq))
        ((>= (* sq sq) size) (list sq sq))
        ((>= (* (1+ sq) sq) size) (list sq (1+ sq)))))))

(defun normalize (str) (map 'string #'char-downcase (remove-if-not #'alphanumericp str)))

(defun array-fill (array str)
  (loop for c across str for i from 0 do (setf (row-major-aref array i) c)))

(defun array-transpose (array)
  (let ((new (make-array (reverse (array-dimensions array)) :initial-element #\Space)))
    (loop for row from 0 below (array-dimension new 0)
      do (loop for col from 0 below (array-dimension new 1) do
        (setf (aref new row col) (aref array col row))))
    new))

(defun array-print-row (array)
  (format nil  "~{~A~^ ~}" (loop for row from 0 below (array-dimension array 0)
    collect (concatenate 'string (
      loop for col from 0 below (array-dimension array 1) 
        collect (aref array row col))))))

(defun encipher (plaintext)
  (setf plaintext (normalize plaintext))
  (if (zerop (length plaintext)) 
    ""
    (let ((a (make-array (get-size plaintext) :initial-element #\Space)))
      (array-fill a plaintext)
      (setf a (array-transpose a))
      (array-print-row a))))
