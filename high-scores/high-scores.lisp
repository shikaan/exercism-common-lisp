(defpackage :high-scores
  (:use :cl)
  (:export :make-high-scores-table :add-player
           :set-score :get-score :remove-player))

(in-package :high-scores)

(defun make-high-scores-table () (make-hash-table)) 

(defun add-player (table player) 
  (setf (gethash player table) 0))

;; Define set-score function
(defun set-score (scores player score)
  (setf (gethash player scores) score))

;; Define get-score function
(defun get-score (scores player) 
  (gethash player scores 0))

;; Define remove-player function
(defun remove-player (scores player)
  (remhash player scores))