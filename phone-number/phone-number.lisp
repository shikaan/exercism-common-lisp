(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defvar *invalid* "0000000000")

(defun invalid-international-code-p (digits l)
  (and (= l 11) (char/= #\1 (char digits 0))))

(defun invalid-area-code-p (10-digits) 
  (position (char 10-digits 0) (list #\0 #\1)))

(defun invalid-exchange-code-p (10-digits) 
  (position (char 10-digits 3) (list #\0 #\1)))

(defun take-last (str n) 
  (let ((l (length str)))
    (if (< l 10) str (subseq str (- l n)))))

(defun invalid-p (all-digits 10-digits)
  (let ((len (length all-digits)))
    (or
      (< len 10)
      (> len 11)
      (invalid-international-code-p all-digits len)
      (invalid-area-code-p 10-digits)
      (invalid-exchange-code-p 10-digits))))

(defun clean (phrase)
  "Converts a PHRASE string into a string of digits.
Will evaluate to \"0000000000\" in case of an invalid input."
  (let ((all-digits (remove-if-not #'digit-char-p phrase)))
    (let ((10-digits (take-last all-digits 10)))
      (if (invalid-p all-digits 10-digits)
        *invalid*
        10-digits))))
