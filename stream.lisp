
(defun cons-stream (left right)
  (cons left
	(delay right)))
(defun car-stream (s)
    (car s))
(defun cdr-stream (s)
    (force (cdr s)))

(defun delay (x)
  #'(lambda () x))
(defun force (x)
  (funcall x))

(defun ones () 
  (cons-stream 1 #'ones))