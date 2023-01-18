(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(defvar *stop-codon* "STOP")

(defun stop-p (i) (string= i *stop-codon*))

(defun protein-from-codon (codon)
  (cond
    ((string= codon "AUG") "Methionine")
    ((or (string= codon "UUU") (string= codon "UUC")) "Phenylalanine")
    ((or (string= codon "UUA") (string= codon "UUG")) "Leucine")
    ((or (string= codon "UCU") (string= codon "UCC") (string= codon "UCA") (string= codon "UCG")) "Serine")
    ((or (string= codon "UAU") (string= codon "UAC")) "Tyrosine")
    ((or (string= codon "UGU") (string= codon "UGC")) "Cysteine")
    ((string= codon "UGG") "Tryptophan")
    ((or (string= codon "UAA") (string= codon "UAG") (string= codon "UGA")) *stop-codon*)))

(defun break-in-blocks-of (s n &optional acc)
  (if (<= (length s) n)
    (append acc (list s))
    (break-in-blocks-of (subseq s n) n (append acc (list (subseq s 0 n))))))

(defun take-until (predicate sequence &key (key identity))
  (loop for i in sequence 
    if (not (funcall predicate (funcall key i))) collect i into result 
    else do (return result)
    finally (return result)))

(defun get-codons (strand) (break-in-blocks-of strand 3))
(defun valid-codon-p (codon) (not (null (protein-from-codon codon))))
(defun valid-codons-p (codons) 
  (every #'valid-codon-p (take-until #'stop-p codons :key #'protein-from-codon)))

(defun valid-nucleotide-p (char)
  (or (char= #\A char) (char= #\U char) (char= #\C char) (char= #\G char)))

(defun valid-strand-p (strand) (every #'valid-nucleotide-p strand))

(defun get-proteins (codons &optional acc)
  (let ((head (protein-from-codon (first codons))))
    (if (or (stop-p head) (null codons))
      acc
      (get-proteins (rest codons) (append acc (list head))))))

(define-condition invalid-protein (condition) ())

(defun proteins (strand)
  (let ((codons (get-codons strand))) 
    (if (and (valid-strand-p strand) (valid-codons-p codons))
      (get-proteins codons)
      (signal 'invalid-protein))))
