(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for :anagram-p :to-list))

(in-package :anagram)

(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
  (remove-if-not #'(lambda (x) (anagram-p subject x)) candidates))

(defun anagram-p (a b)
  (let (
    (list-a (to-list a))
    (list-b (to-list b)))
    (unless (equalp list-a list-b)
      (equalp (sort list-a #'char<) (sort list-b #'char<)))))

(defun to-list (a)
  (coerce (format nil "~(~a~)" a) 'list))