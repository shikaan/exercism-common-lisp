(defpackage :affine-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :affine-cipher)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")
(defvar *size* (length *alphabet*))

(defun empty-p (s) (zerop (length s)))

(defun join (s1 s2)
  (format nil "~{~A~^ ~}" (remove-if #'empty-p (list s1 s2))))

(defun append-char (s c)
  (format nil "~A~A" (or s "") (or c "")))

(defun words-of-size (n s &optional (initial-value nil))
  "Returns a string s as words of size n"
  (if (<= (length s) n) 
    (join initial-value s)
    (words-of-size n (subseq s n) (join initial-value (subseq s 0 n)))))

(defun unwords (s) 
  "Returns a string of words as one word (i.e., removes spaces)"
  (remove #\Space s))

(defun char-index (c) 
  "Returns char index in the alphabet"
  (position (char-downcase c) *alphabet*))

(defun make-encode-char (a b)
  "Returns a callback to encode a character for a given key"
  (lambda (c)
    (cond 
      ((alpha-char-p c) (char *alphabet* (mod (+ (* a (char-index c)) b) *size*)))
      ((digit-char-p c) c))))

(defun mmi (a m) 
  "Finds the multiplicative inverse of a mod m"
  (loop for i from 1 to m if (= 1 (mod (* a i) m)) do (return i)))

(defun make-decode-char (a b)
  "Returns a callback to encode a character for a given key"
  (lambda (c)
    (cond 
      ((alpha-char-p c) (char *alphabet* (mod (* (mmi a *size*) (- (char-index c) b)) *size*)))
      ((digit-char-p c) c))))

(defun decrypts-p (a) 
  "Returns true if the provided key allow decryption"
  (= 1 (gcd a *size*)))

(defun decode (ciphertext a b) 
  (let ((decode-char (make-decode-char a b)))
    (if (decrypts-p a)
      (map 'string decode-char (unwords ciphertext)))))

(defun encode (plaintext a b)
  (let ((encode-char (make-encode-char a b)))
    (if (decrypts-p a)
      (words-of-size 5
        (reduce #'append-char (map 'list encode-char plaintext))))))
