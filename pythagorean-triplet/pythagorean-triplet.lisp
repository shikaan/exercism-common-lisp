(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplet-p (a b c)
  (and
   (< a b c)
   (= (expt c 2) (+ (expt a 2) (expt b 2)))))

(defun to-be-picked-p (a b c n)
  (and (= n (+ a b c)) (triplet-p a b c)))

(defun triplets-with-sum (n)
  (let ((result nil) (l 0) (r 0))
    (loop for i from 0 to n 
          do (progn 
              (setf l (1+ i))
              (setf r (1- n))
              (loop while (< l r) 
                do (cond 
                  ((to-be-picked-p i l r n) (progn (push (list i l r) result) (setf r (1- r))))
                  ((> n (+ i l r)) (setf l (1+ l)))
                  (t (setf r (1- r)))))))
    (reverse result)))
