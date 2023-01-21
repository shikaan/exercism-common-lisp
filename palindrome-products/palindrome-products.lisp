(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun palindrome-p (n)
  (let ((s (write-to-string n)))
    (string= s (reverse s))))

(defun divides-p (a b) (zerop (mod a b)))

(defun get-factors (n min-factor max-factor)
  (loop 
    for candidate from min-factor to (isqrt n)
    for q = (/ n candidate)
    if (and (divides-p n candidate) (<= min-factor q max-factor))
      collect (list candidate q)))

(defun smallest (min-factor max-factor)
  (if (< min-factor max-factor)
    (loop 
      for i from (expt min-factor 2) to (expt max-factor 2)
      if (and (palindrome-p i) (get-factors i min-factor max-factor))
        return (values i (get-factors i min-factor max-factor)))))

(defun largest (min-factor max-factor)
  (if (< min-factor max-factor)
    (loop 
      for i from (expt max-factor 2) downto (expt min-factor 2)
      if (and (palindrome-p i) (get-factors i min-factor max-factor))
        return (values i (get-factors i min-factor max-factor)))))