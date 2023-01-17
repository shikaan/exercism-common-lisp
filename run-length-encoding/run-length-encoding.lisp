(defpackage :run-length-encoding
  (:use :cl)
  (:export :encode
           :decode))

(in-package :run-length-encoding)

(defun empty-p (seq) 
  "Returns t if a sequence is empty"
  (zerop (length seq)))

(defun concat (s1 s2) (format nil "~a~a" s1 s2))

(defun split-if (test str)
  "Splits a sequence on every item satisfying the test"
  (loop
    for el across str with result with subs = ""
    if (not (funcall test el)) 
      do (setf subs (concat subs el))
    else 
      do (progn (push (concat subs el) result) (setf subs ""))
    finally 
      (progn (unless (empty-p subs) (push subs result)) (return (reverse result)))))

(defun group-by (test str)
  "Groups chars in a string based on a test"
  (reverse
    (reduce
      #'(lambda (result element)
          (let* ((most-recent (first result)) (most-recent-element (first most-recent)))
            (if (or (null most-recent-element) (funcall test element most-recent-element))
              (cons (push element most-recent) (rest result))
              (push (list element) result))))
      str
      :initial-value '())))

(defun encode-sequence (l)
  "Makes a sequence of type (x, ...x) -> Nx"
  (if (= (length l) 1)
      (format nil "~a" (first l))
      (concat (length l) (first l))))

(defun decode-symbol (s)
  "Decode a symbol of type Nx into a string xxx...x"
  (let ((size (length s)))
    (let ((c (char s (1- size))) (times (if (= 1 size) 1 (parse-integer (subseq s 0 (1- size))))))
      (coerce (loop for i from 1 to times collect c) 'string))))

(defun encode (plain)
  (reduce #'concat
    (group-by #'char= plain)
    :key #'encode-sequence
    :initial-value ""))

(defun allowed-char (x) (or (alpha-char-p x) (char= x #\Space)))

(defun decode (compressed)
  (reduce #'concat
    (split-if #'allowed-char compressed)
    :key #'decode-symbol
    :initial-value ""))
