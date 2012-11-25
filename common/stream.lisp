(defvar the-empty-stream '())
(defun stream-null? (s)
  (eql nil s))
(defun force (delayed-object)
  (funcall delayed-object))

(defun stream-car (s)
  (car s))

(defun stream-cdr (s)
  (force (cdr s)))

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))
;please notice this is really an efficiency problem
;(defmacro delay (x)
 ; `(lambda () ,x))
(defmacro delay (expr)`(memo-proc (lambda () ,expr)))

(defun memo-proc (proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
        (progn
          (setf result (funcall proc))
          (setf already-run? t)
          result)
        result))))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun stream-filter (pred s)
  (cond ((stream-null? s) the-empty-stream)
	((funcall pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(t (stream-filter pred (stream-cdr s)))))
(defun stream-enumerate-interval (m n)
  (if (> m n)
      the-empty-stream
      (cons-stream m 
		   (stream-enumerate-interval (+ 1 m) n))))
(defun integer-from (n)
  (cons-stream n
	       (integer-from (+ n 1))))
(defvar primes (stream-filter #'prime? (integer-from 2)))
(defun display-stream (s n)
  (cond ((< n 0) t)
	(t (print (stream-car s))
	   (display-stream (stream-cdr s) (- n 1)))))
(defun stream-map (proc &rest ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car ss))
       (apply #'stream-map
	      (cons proc (mapcar #'stream-cdr ss))))))
(defun add-stream (s1 s2)
  (stream-map #'+ s1 s2))
(defvar fibs 
  (cons-stream 0 
	       (cons-stream 1
			    (add-stream (stream-cdr fibs) fibs))))
(defun sieve (s)
  (cons-stream 
   (stream-car s)
   (sieve (stream-filter 
	   #'(lambda (x)
	     (not (= (mod x (stream-car s)) 0)))
	   (stream-cdr s)))))

(defun stream-map-element (proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (funcall proc (stream-car s))
		   (stream-map-element proc (stream-cdr s)))))
(defun scale-stream (factor s)
  (stream-map-element #'(lambda (x) (* factor x)) s))

(defun integral (integrand init-value dt)
  (let ((int nil))
    (setf int (cons-stream init-value
			   (add-stream int
				       (scale-stream dt integrand))))
    int))
