(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

(defun make-school () (make-hash-table))

(defun has-student-p (school student)
  (loop for students being the hash-values of school 
    do (if (position student students :test #'string=) (return t))))

(defun prepare-grade (grade new-student)
  (remove-duplicates (sort (append grade (list new-student)) #'string<) :test #'string=))

(defun add (school student grade)
  (unless (has-student-p school student)
    (setf (gethash grade school) (prepare-grade (gethash grade school '()) student))))

(defun grade (school n) (gethash n school '()))

(defun flatten (l) (reduce #'append l :initial-value '()))
(defun hash-keys (h) (loop for k being the hash-keys of h collect k))

(defun roster (school)
  (let ((keys (sort (hash-keys school) #'<)))
    (flatten 
        (mapcar #'(lambda (key) (gethash key school)) keys))))