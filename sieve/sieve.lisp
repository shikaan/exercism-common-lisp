(defpackage :sieve
  (:use :cl)
  (:export :primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package :sieve)

(defun make-range (n) 
  "Returns the list of the number from 2 to n"
  (loop for i from 2 to n collect i))

(defun last-el (list) 
  "Returns last elements in the list"
  (first (last list)))

(defun multiples-of (n limit) 
  "Returns all the multiples of n up to the limit"
  (rest (loop for i from n to limit by n collect i)))

(defun next-candidate (candidates previous-candidate) 
  "Returns next candidate from a candidate list"
  (loop for c in candidates if (> c previous-candidate) do (return c)))

(defun remove-multiples-of (list n)
  "Removes multiples of n from list"
  (reverse (set-difference list (multiples-of n (last-el list))))
)

(defun primes-in-range-from (range current)
  "Returns all the primes in a range greater than current"
  (if (null current) range
    (let 
      ((candidates (remove-multiples-of range current)))
      (primes-in-range-from candidates (next-candidate candidates current)))))

(defun primes-to (n)
  "List primes up to `n' using sieve of Eratosthenes."
  (if (> n 1)
    (primes-in-range-from (make-range n) 2)))
