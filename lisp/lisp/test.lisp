(defun test ()
  (format t "hello world, fuck"))

(defun Harmony (k)
  (if (= k 1)
      1.0
      (+ (/ 1.0 k) (Harmony (- k 1)))))


(defun accumulate (combiner term start next end nil-value)
  (if (> start end)
      nil-value
      (funcall combiner
	       (funcall term start)
	       (accumulate combiner 
			   term 
			   (funcall next start)
			   next
			   end
			   nil-value))))

(defun H-from-1-n (n)
  (accumulate #'+ #'Harmony 1 #'(lambda (x) (+ x 1)) n 0))

(defun another-H-from-1-n (n)
  (- (* n (Harmony n)) n))
