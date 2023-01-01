(defpackage :acronym
  (:use :cl)
  (:export :acronym))

(in-package :acronym)

(require :uiop)

(defun upcase-initial (str) (char-upcase (char str 0)))

(defun acronym (str)
  "Returns the acronym for a noun of tech jargon."
  (let ((words (uiop:split-string str :separator " -")))
    (map 'string #'upcase-initial words)))
