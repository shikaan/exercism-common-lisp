(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)

(defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (map 'string #'complementary-nucleotide str))

(defun complementary-nucleotide (c)
  (case c
    (#\G #\C)
    (#\C #\G)
    (#\T #\A)
    (#\A #\U)))