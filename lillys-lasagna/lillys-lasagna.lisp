(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven () 
  "how many minutes the lasagna should be in the oven" 
  337)

(defun remaining-minutes-in-oven (n) 
  "how many minutes the lasagna still has to remain in the oven"
  (- (expected-time-in-oven) n))

(defun preparation-time-in-minutes (layers) 
  "how many minutes Lilly spent preparing the lasagna"
  (* layers 19))

(defun elapsed-time-in-minutes (layers minutes-so-far)
  "how many minutes Lilly has worked on cooking the lasagna"
  (+ (preparation-time-in-minutes layers) minutes-so-far))
