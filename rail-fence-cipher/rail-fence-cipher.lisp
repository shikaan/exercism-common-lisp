(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun clean-input (msg) (remove #\Space msg))

(defun nth-even (n) (* 2 n))

(defun skip-pattern (rail rails)
  (cond 
    ((or (zerop rail) (= rail (1- rails))) (list (nth-even (1- rails))))
    (t (list (nth-even (- rails (1+ rail))) (nth-even rail)))))

(defun collect-chars-at-rail (str rail rails)
  (let ((pattern (skip-pattern rail rails)))
  (loop for i from rail below (length str)
        for j from 0
        for c = (char str i)
        with next = 0
        if (= next j)
        collect c and do 
          (progn 
            (setf next (+ next (first pattern)))
            (setf pattern (reverse pattern))))))

(defun encode (msg rails)
  (setf msg (clean-input msg))
  (loop for r from 0 below rails with result
    do (setf result (append result (collect-chars-at-rail msg r rails)))
    finally (return (coerce result 'string))))

(defun insert-at (str c pos) (setf (aref str pos) c))

(defun decode (msg rails)
  (let* ((result (make-string (length msg))) (base (nth-even (1- rails))))

  ))

; f(0, 4) = 0
; f(1, 4) = 6       pattern-1
; f(2, 4) = 12      2x pattern-1
; f(3, 4) = 18      3x pattern-1
; f(4, 4) = 1       rail + pattern-1
; f(5, 4) = 5
; f(6, 4) = 7