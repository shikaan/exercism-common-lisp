(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

(defvar *dows* (list :monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defun divisible-p (n f) (zerop (mod n f)))

(defun leap-year-p (year)
  (and
    (divisible-p year 4) 
    (or
      (and (divisible-p year 100) (divisible-p year 400))
      (not (divisible-p year 100)))))

(defun dow-of-first (month year)
  "Returns dow of the first day of month/year"
  (nth 6 (multiple-value-list 
    (decode-universal-time 
      (encode-universal-time 0 0 0 1 month year)))))

(defun first-occurence-of (dow dow-of-first)
  "Return the date of the first dow (0-6) knowing the first day of the month"
  (1+ (mod (- dow dow-of-first) 7)))

(defun last-day-of (month year)
  "Returns last day of the month/year"
  (cond
    ((= 2 month) (if (leap-year-p year) 29 28))
    ((position month '(1 3 5 7 8 10 12)) 31)
    (t 30)))

(defun dates-of-dow (dow month year)
  "Returns a list of dates for a gives dow in a month/year"
  (loop for i from (first-occurence-of dow (dow-of-first month year)) to (last-day-of month year) by 7 collect i))

(defun dow-to-index (dow) (position dow *dows*))

(defun teenth-p (n) (and (>= n 13) (<= n 19)))

(defun schedule-to-fn (schedule)
  (case schedule
    (:first #'first)
    (:second #'second)
    (:third #'third)
    (:fourth #'fourth)
    (:last #'(lambda (l) (first (last l))))
    (:teenth #'(lambda (l) (find-if #'teenth-p l)))))

(defun meetup (month year dow schedule)
  "Returns a date in the format (y m d) for a given meetup date."
  (let ((dates (dates-of-dow (dow-to-index dow) month year)))
    (list 
      year
      month
      (funcall (schedule-to-fn schedule) dates))))

