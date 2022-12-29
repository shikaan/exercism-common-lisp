(defpackage :word-count
  (:use :cl)
  (:export :count-words :split-if :split-if-not))
(in-package :word-count)

(defun empty-p (s) (zerop (length s)))
(defun word-char-p (c) (or (alphanumericp c) (char= c #\')))

(defun split-if (str test &optional accumulator)
  (if (empty-p str)
    accumulator
    (let ((chunk-end (position-if test str)) (substr nil) (total-length (length str)))
      (if (null chunk-end) (setf chunk-end total-length))
      (setf substr (subseq str (min (1+ chunk-end) total-length)))
      (let ((chunk (subseq str 0 chunk-end)))
        (unless (empty-p chunk) (push chunk accumulator))
        (split-if substr test accumulator)))))

(defun split-if-not (str test) (split-if str #'(lambda (x) (not (funcall test x)))))

(defun normalize (word) (string-trim "'" (string-downcase word)))

(defun count-words (sentence)
  (let ((words (mapcar #'normalize (split-if-not sentence #'word-char-p))))
    (loop 
      for key in (remove-duplicates (copy-seq words) :test #'string=)
      for value = (count key words :test #'string=)
      unless (empty-p key) 
      collect (cons key value))))