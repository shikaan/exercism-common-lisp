(defpackage :collatz-conjecture
  (:use :cl)
  (:export :collatz))

(in-package :collatz-conjecture)

(defun collatz (n &optional (counter 0))
  (cond ((<= n 0) nil)
        ((= 1 n) counter)
        ((evenp n) (collatz (/ n 2) (1+ counter)))
        ((oddp n) (collatz (1+ (* 3 n)) (1+ counter)))))
