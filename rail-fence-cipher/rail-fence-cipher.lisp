(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun clean-input (msg) (remove #\Space msg))

(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

(defun up-down (n cap) 
  (let* (
    (b (append (loop for i from 1 to n collect i) (loop for i from (1- n) downto 2 collect i)))
    (base (circular! b)))
    (loop for i from 0 to cap collect (nth i base))))

(defun encode-list (l rails)
  (mapcar #'first
      (sort 
        (mapcar #'list 
          l 
          (up-down rails (length l))) #'< :key #'second)))

(defun encode (msg rails)
  (setf msg (clean-input msg))
  (coerce (encode-list (coerce msg 'list) rails) 'string))

(defun decode (msg rails)
  (coerce (mapcar #'first
    (sort 
      (mapcar #'list 
        (coerce msg 'list) 
        (encode-list (loop for i from 1 to (length msg) collect i) rails)) #'< :key #'second)) 'string))