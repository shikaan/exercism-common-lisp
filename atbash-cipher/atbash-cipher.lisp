(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode))

(in-package :atbash-cipher)

(defvar *latin* "abcdefghijklmnopqrstuvwxyz")
(defvar *atbash* "zyxwvutsrqponmlkjihgfedcba")

(defun encode-char (c) 
  (let ((p (position (char-downcase c) *latin*)))
    (if (null p) c (char *atbash* p))))

(defun clean (str) (remove-if-not #'alphanumericp str))

(defun take (seq n) 
  (setf n (min n (length seq)))
  (values (subseq seq 0 n) (subseq seq n)))

(defun unwords-at (str size &optional accumulator)
  (if (zerop (length str))
    accumulator
    (multiple-value-bind (taken remainder) (take str size)
      (unwords-at 
        remainder 
        size 
        (if accumulator (format nil "~a ~a" accumulator taken) taken)))))

(defun encode (plaintext)
  (unwords-at (map 'string #'encode-char (clean plaintext)) 5))
