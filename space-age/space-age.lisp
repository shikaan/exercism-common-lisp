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

(defvar earth-seconds 31557600)

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

(defun years-on (age planet) (/ age (* earth-seconds (factor planet))))

(defun on-earth (age) (years-on age :earth))
(defun on-mercury (age) (years-on age :mercury))
(defun on-venus (age) (years-on age :venus))
(defun on-mars (age) (years-on age :mars))
(defun on-jupiter (age) (years-on age :jupiter))
(defun on-saturn (age) (years-on age :saturn))
(defun on-uranus (age) (years-on age :uranus))
(defun on-neptune (age) (years-on age :neptune))