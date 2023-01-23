(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defvar *moves* '("wink" "double blink" "close your eyes" "jump"))

(defun reverse-p (decoded)
  (and (= 5 (length decoded)) (char= #\1 (char decoded 4))))

(defun commands (number)
  (let* (
    (decoded (reverse (format nil "~b" number)))
    (result (loop 
      for c across decoded
      for move in *moves*
      if (and move (char= c #\1)) collect move)))
    (if (reverse-p decoded) (reverse result) result)))
