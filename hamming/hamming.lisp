(defpackage :hamming
  (:use :cl)
  (:export :distance))

(in-package :hamming)

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands."
  (if (= (length dna1) (length dna2))
    (length (remove-if-not #'null (map 'list #'char= dna1 dna2)))))
