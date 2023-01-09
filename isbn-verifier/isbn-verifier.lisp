(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

(defun parse-char (c)
  (case c
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    (#\x 10)
    (#\X 10)))

(defun valid-char-p (c) 
  (or 
    (digit-char-p c) 
    (char= #\x (char-downcase c))))

(defun valid-clean-string-p (str)
  (and
    (= 10 (length str))
    (every #'digit-char-p (subseq str 0 9))
    (valid-char-p (char str 9))))

(defun clean (string)
  (remove-if-not #'alphanumericp string))

(defun validp (isbn)
  (setf isbn (clean isbn))
  (if (valid-clean-string-p isbn)
    (zerop
      (mod
        (loop 
          for d in (reverse (map 'list #'parse-char isbn))
          for i from 1 
            sum (* i d))
        11))))
