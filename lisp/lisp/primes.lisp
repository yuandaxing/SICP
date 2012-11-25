(defun square (x) (* x x))
(defun prime-1? (x)
  (defun iter (i)
    (cond ((> (square i) x) t)
	  ((= (mod x i) 0) nil)
	  (t (iter (+ 1 i)))))
  (iter 2))

(defun expmod (base exp m)
    (cond ((= exp 0) 1)
	  ((evenp exp) (mod 
			(square (expmod base (/ exp 2) m))
			m))
	  (t (mod (* base
		     (expmod base (- exp 1) m))
		  m))))


(defun fermat-test (n)
  (let ((r (+ (random (- n 1)) 1)))
    (= (mod r n ) (expmod r n n))))


(defun fermat-test-n? (x &optional (count 20))
  (defun iter-2 (i)
    (cond ((> i count) t)
	  ((fermat-test x) (iter-2 (+ i 1)))
	  (t nil))))



(defun detect-fermat-error (s e)
  (if (> s e)
      t
      (progn 
	(if (not (eql (fermat-test-n? s) (prime-1? s)))
	  (format t "fermat error for ~a~%" s))
	(detect-fermat-error (+ s 1) e))))
(defun detect-miller-rabin-error (s e)
  (if (> s e)
      t
      (progn 
	(if (not (eql (miller-test s) (prime-1? s)))
	  (format t "miller rabin test error for ~a~%" s))
	(detect-miller-rabin-error (+ s 1) e))))

;n must not 0
(defun miller-rabin-expmod (base exp n) 
  (cond ((= exp 0) 1)
	((evenp exp)
	 (let ((mre (miller-rabin-expmod base (/ exp 2) n)))
	   (if (and (not (= mre 1)) (not (= mre (- n 1)))
		    (= (mod (square mre) n) 1))
	       0
	       (mod (square mre) n))))
	(t (mod (* base (miller-rabin-expmod base (- exp 1) n)) n))))


(defun miller-test-1 (n)
  (let ((tn (+ (random (- n 1)) 1)))
    (= (miller-rabin-expmod tn n n) tn)))

(defun miller-test (n &optional (count 20))
  (defun iter-1 (i)
    (cond ((> i count) t)
	  ((not (miller-test-1 n)) nil)
	  (t (iter-1 (+ i 1)))))
  (iter-1 1))
