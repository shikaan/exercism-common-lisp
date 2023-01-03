(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)

(defun separator-p (c) (or (char= c #\Space) (char= c #\-)))

(defun clean (s)
  (map 'string #'char-downcase (remove-if #'separator-p s)))

(defun isogram-p (string)
  "Is string an Isogram?"
  (string= (clean string) (remove-duplicates (clean string))))
