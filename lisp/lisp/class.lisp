(load "~/scheme/common/math.lisp")
(load "~/scheme/common/stream.lisp")
(defclass key-value ()
  ((key
    :initarg :key
    :accessor key
    :initform (error "Must supply :word"))
   (value
    :initarg :value
    :accessor value
    :initform nil)))

(defun make-kv (key value)
  (make-instance 'key-value :key key :value value)
  (make-instance 'key-value :key key :value value))

(defvar *kv-store* (make-hash-table :test #'equal))


(defun put (k v)
  (setf (gethash k *kv-store*)
	(make-kv k v)))
(defun get-kv (k)
  (gethash k *kv-store*))
(defun put-number-pair (n)
  (let ((sp (stream-map-element #'(lambda (x) (put x x))
				primes)))
    (defun iter-p (i)
      (cond ((> i n) (return-from put-number-pair t))
	    (t (progn
		 (stream-ref sp i)
		 (iter-p (+ i 1))))))
    (iter-p 0)))