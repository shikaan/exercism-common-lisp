(defpackage :space-age
  (:use :cl)
  (:export :on-mercury
           :on-venus
           :on-earth
           :on-mars
           :on-jupiter
           :on-saturn
           :on-uranus
           :on-neptune))

(in-package :space-age)

(defconstant earth-seconds 31557600)
(defconstant planets (list :mercury :venus :earth :mars :jupiter :saturn :uranus :neptune))

(defun factor (planet)
  (case planet
    (:mercury 0.2408467)
    (:venus 0.61519726)
    (:earth 1.0)
    (:mars 1.8808158)
    (:jupiter 11.8808158)
    (:saturn 29.447498)
    (:uranus 84.016846)
    (:neptune 164.79132)))

(defun years-on (planet) (lambda (age) (/ age (* earth-seconds (factor planet)))))

(loop for planet in planets 
  for fname = (intern (format nil "ON-~a" planet))
  do (setf (fdefinition fname) (years-on planet)))