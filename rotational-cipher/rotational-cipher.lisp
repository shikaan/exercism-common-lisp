(defpackage :rotational-cipher
  (:use :cl)
  (:export :rotate))

(in-package :rotational-cipher)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun rotate-char (key)
  (lambda (x)
    (let ((p (position (char-downcase x) *alphabet*)))
      (if p
        (let ((c (char *alphabet* (mod (+ p key) 26))))
          (if (upper-case-p x) (char-upcase c) c))
        x))))

(defun rotate (text key)
  (map 'string (rotate-char key) text))
