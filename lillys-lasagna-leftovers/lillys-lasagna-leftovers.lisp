(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

(defun preparation-time (&rest rest) (* 19 (length rest)))

(defconstant base-remaining-minutes 337)

(defun remaining-minutes-in-oven (&optional (variation :normal))
  (case variation
    (:normal base-remaining-minutes)
    (:shorter (- base-remaining-minutes 100))
    (:very-short (- base-remaining-minutes 200))
    (:longer (+ base-remaining-minutes 100))
    (:very-long (+ base-remaining-minutes 200))
    (otherwise 0)))

(defun split-leftovers (&key (weight nil weight-supplied-p) (human 10) (alien 10))
  (cond
    ((not weight-supplied-p) :just-split-it)
    ((and (null weight) weight-supplied-p) :looks-like-someone-was-hungry)
    (t (- weight (+ human alien)))))
