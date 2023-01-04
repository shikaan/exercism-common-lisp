(defpackage :nucleotide-count
  (:use :cl)
  (:export :dna-count :nucleotide-counts :invalid-nucleotide))

(in-package :nucleotide-count)

(defun valid-nucleotide-p (nucleotide) (position nucleotide (list #\A #\T #\C #\G)))
(defun valid-strand-p (strand) (every #'valid-nucleotide-p strand))

(define-condition invalid-nucleotide (error) ())

(defun dna-count (nucleotide strand)
  "Returns a count of the given nucleotide appearing in a DNA strand."
  (if (valid-nucleotide-p nucleotide) 
    (- (length strand) (length (remove nucleotide strand))) 
    (error 'invalid-nucleotide)))

(defun nucleotide-counts (strand)
  "Returns a hash of nucleotides and their counts in a given DNA strand."
  (let ((result (make-hash-table)))
    (setf (gethash #\A result) (dna-count #\A strand))
    (setf (gethash #\C result) (dna-count #\C strand))
    (setf (gethash #\G result) (dna-count #\G strand))
    (setf (gethash #\T result) (dna-count #\T strand))
    result))
