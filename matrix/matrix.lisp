(defpackage :matrix
  (:use :cl)
  (:export :row
           :column))

(in-package :matrix)
(require :uiop)

(defun empty-p (s) (zerop (length s)))

(defun make-matrix (input)
  (if (= 1 (length input))
    (make-array '(1 1) :initial-element (parse-integer input))
    (let* (
      (r (uiop:split-string input :separator (list #\Newline)))
      (rows (1- (length r)))
      (cols (length (uiop:split-string (first r) :separator (list #\Space))))
      (matrix (make-array (list rows cols))))
      (loop
        for j from 0 below (array-total-size matrix)
        for i in (mapcar #'parse-integer (remove-if #'empty-p (uiop:split-string input :separator (list #\Space #\Newline))))
        do (setf (row-major-aref matrix j) i))
      matrix)))

(defun row (input-matrix index)
  (decf index)
  (let ((m (make-matrix input-matrix)))
    (loop for i from 0 below (array-dimension m 1) collect (aref m index i))))

(defun column (input-matrix index)
  (decf index)
  (let ((m (make-matrix input-matrix)))
    (loop for i from 0 below (array-dimension m 0) collect (aref m i index))))
