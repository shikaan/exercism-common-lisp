(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun palindrome-p (n)
  (let ((s (format nil "~a" n)))
    (string= s (reverse s))))

(defun palindrome-between (min max)
  (loop for i from min to max if (palindrome-p i) collect i))

(defun palindrome-between-2 (min max)
  (loop for i from max downto min if (palindrome-p i) collect i))

(defun get-factors (n minimum maximum)
  (loop with result for a from minimum to (min n maximum)
    do (loop for b from a to (min n maximum)
      if (= (* a b) n) do (push (list a b) result))
    finally (return (reverse result))))

(defun smallest (min-factor max-factor)
  (if (< min-factor max-factor)
    (let ((palindromes (palindrome-between (expt min-factor 2) (expt max-factor 2))))
      (loop for i in palindromes for factors = (get-factors i min-factor max-factor) 
        if factors do (return (values i factors))))))

(defun largest (min-factor max-factor)
  (if (< min-factor max-factor)
    (let ((palindromes (palindrome-between-2 (expt min-factor 2) (expt max-factor 2))))
      (loop for i in palindromes for factors = (get-factors i min-factor max-factor)
        if factors do (return (values i factors))))))
