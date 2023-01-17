(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defun append-item (seq item) (append seq (list item)))

(defun chunk (seq size &optional accumulator)
  (if (<= (length seq) size)
    (append-item accumulator seq)
    (chunk
      (subseq seq 1)
      size
      (append-item accumulator (subseq seq 0 size)))))

(defun multiply-chunk (chunk)
  (reduce #'* chunk :initial-value 1 :key #'digit-char-p))

(defun invalid-p (digits span)
  (or 
    (< span 0)
    (> span (length digits))
    (notevery #'digit-char-p digits)))

(defun largest-product (digits span)
  (unless (invalid-p digits span)
    (loop for group in (chunk digits span) maximize (multiply-chunk group))))
