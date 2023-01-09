(defpackage :matching-brackets
  (:use :cl)
  (:export :pairedp))

(in-package :matching-brackets)

(defun pairedp (value)
  (let ((looking-for '()))
    (loop 
      for c across value 
      for searched = (first looking-for)
      for s = (string c)
        do (cond
          ((string= s "(") (push ")" looking-for))
          ((string= s "[") (push "]" looking-for))
          ((string= s "{") (push "}" looking-for))
          ((or (string= s ")") (string= s "]") (string= s "}")) 
            (if (and searched (string= s searched)) 
              (setf looking-for (rest looking-for))
              (progn (setf looking-for '(:error)) (return))))))
    (null looking-for)))
