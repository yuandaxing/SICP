

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
