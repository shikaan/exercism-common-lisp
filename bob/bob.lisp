(defpackage :bob
  (:use :cl)
  (:export :response))
(in-package :bob)

(defun empty-p (s) (string= "" s))
(defun yell-p (s) 
  (let ((letters (remove-if-not #'alpha-char-p s)))
    (and (not (empty-p letters)) (every #'upper-case-p letters))))
(defun question-p (s) (and (not (empty-p s)) (char= #\? (char s (1- (length s))))))

(defun response (hey-bob)
  (let ((s (string-trim '(#\Space #\Newline #\Tab) hey-bob)))
    (cond
        ((yell-p s)
          (if (question-p s) 
            "Calm down, I know what I'm doing!"
            "Whoa, chill out!"))
        ((question-p s) "Sure.")
        ((empty-p s) "Fine. Be that way!")
        (t "Whatever."))))
