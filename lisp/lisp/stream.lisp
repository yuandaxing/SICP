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


(defun stream-enumerate-interval (m n)
  (if (> m n)
      the-empty-stream
      (cons-stream m 
		   (stream-enumerate-interval (+ 1 m) n))))

(defvar s-1-10 (stream-enumerate-interval 1 10))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun integer-from (n)
  (cons-stream n
	       (integer-from (+ n 1))))

(defvar integer (integer-from 1))

(defun stream-filter (pred s)
  (cond ((stream-null? s) the-empty-stream)
	((funcall pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(t (stream-filter pred (stream-cdr s)))))
      
(defun prime? (n)
  (defun iter (i)
    (cond ((> (* i i) n) t)
	  ((= (mod n i) 0) nil)
	  (t (iter (+ i 1)))))
  (iter 2))

(defvar primes (stream-filter #'prime? (integer-from 2)))


;(defun stream-filter #(lambda (x) (= (GCD (x n)) 1))

(defun display-stream (s n)
  (cond ((< n 0) t)
	(t (print (stream-car s))
	   (display-stream (stream-cdr s) (- n 1)))))

(defvar ones (cons-stream 1 ones))

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

(defun iter-fib (n)
    (defun iter-1 (c ne i)
      (if (= i 0)
	  c
	  (iter-1 ne (+ c ne) (- i 1))))
  (iter-1 0 1 n))


(defun GCD-1 (a b)
  (if (= b 0) 
      a
      (GCD-1 b (mod a b))))


(defvar s1 (stream-enumerate-interval 10 1000))
(defvar s2 (stream-enumerate-interval 20 1020))
(defvar s3 (stream-enumerate-interval 30 1030))

(defvar ss (stream-map #'+ s1 s2 s3))

;(stream-ref ss 200)

(defun sieve (s)
  (cons-stream 
   (stream-car s)
   (sieve (stream-filter 
	   #'(lambda (x)
	     (not (= (mod x (stream-car s)) 0)))
	   (stream-cdr s)))))

(defvar prime-2 (sieve (integer-from 2)))

(defvar integers-2 (cons-stream 1
				(add-stream integers-2 ones)))

(defvar s22 (cons-stream 1 (add-stream s22 s22)))

(defmacro mul-stream (&rest ss)
  `(stream-map #'* ,@ss))

(defvar s*s (mul-stream s22 s22))