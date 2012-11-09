

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

(defun compose (f g)
  #'(lambda (x)
    (funcall f (funcall g x))))
(defun compose-n (f n)
  (if (= n 1)
      f
      (compose f
	       (compose-n f (- n 1)))))
(defun f (x) (* x x))
(defun g (x) (* x 2))