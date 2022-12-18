(defpackage :pal-picker
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

(in-package :pal-picker)

(defun pal-picker (personality) 
  (case personality
    (:lazy "Cat")
    (:energetic "Dog")
    (:quiet "Fish")
    (:hungry "Rabbit")
    (:talkative "Bird")
    (otherwise "I don't know... A dragon?")))

(defun betweenp (n a b) (and (>= n a) (<= n b)))

(defun habitat-fitter (weight)
  (cond
    ((>= weight 40) :massive)
    ((betweenp weight 20 39) :large)
    ((betweenp weight 10 19) :medium)
    ((betweenp weight 1 9) :small)
    ((<= weight 0) :just-your-imagination)))

(defun feeding-time-p (fullness) 
  (if (> fullness 20) "All is well." "It's feeding time!"))

(defun pet (pet) (if (equalp pet "Fish") "Maybe not with this pet..."))

(defun play-fetch (pet) (unless (equalp pet "Dog") "Maybe not with this pet..."))
