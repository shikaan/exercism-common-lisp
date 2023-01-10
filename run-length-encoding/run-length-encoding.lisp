(defpackage :run-length-encoding
  (:use :cl)
  (:export :encode
           :decode))

(in-package :run-length-encoding)

(defun empty-p (seq) 
  "Returns t if a sequence is empty"
  (zerop (length seq)))

(defun split-if (test seq &optional accumulator)
  "Splits a sequence on every item satisfying the test"
  (if (empty-p seq)
      (reverse accumulator)
      (let ((chunk-end (position-if test seq)) (subs nil) (total-length (length seq)))
        (if (null chunk-end) (setf chunk-end total-length))
        (setf subs (subseq seq (min (1+ chunk-end) total-length)))
        (let ((chunk (subseq seq 0 (1+ chunk-end))))
          (unless (empty-p chunk) (push chunk accumulator))
          (split-if test subs accumulator)))))

(defun group-by (test str)
  "Groups chars in a string based on a test"
  (reverse
    (reduce
        #'(lambda (acc c)
            (let ((most-recent (first acc)))
              (let ((group (first most-recent)))
                (if (or (null group) (funcall test c group))
                    (cons (push c most-recent) (rest acc))
                    (push (list c) acc)))))
      str
      :initial-value '())))

(defun encode-sequence (l)
  "Makes a sequence of type (x, ...x) -> Nx"
  (if (= (length l) 1)
      (format nil "~a" (first l))
      (format nil "~a~a" (length l) (first l))))

(defun decode-symbol (s)
  "Decode a symbol of type Nx into a string xxx...x"
  (let ((size (length s)))
    (let ((c (char s (1- size))) (times (if (= 1 size) 1 (parse-integer (subseq s 0 (1- size))))))
      (coerce (loop for i from 1 to times collect c) 'string))))


(defun encode (plain)
  (if (empty-p plain)
      ""
      (reduce 
          #'(lambda (acc l)
              (format nil "~a~a" acc (encode-sequence l)))
        (group-by #'char= plain)
        :initial-value "")))

(defun allowed-char (x) (or (alpha-char-p x) (char= x #\Space)))

(defun decode (compressed)
  (if (empty-p compressed)
      ""
      (reduce
          #'(lambda (acc s)
              (format nil "~a~a" acc (decode-symbol s)))
        (split-if #'allowed-char compressed) 
        :initial-value "")))
