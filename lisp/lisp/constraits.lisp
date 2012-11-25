(defun queue-col (col size)
  (if (= col 0)
      (list empty-board)
      (filter
       #'(lambda (positions) (safe? col positions))
       (flatmap
	#'(lambda (rest-of-queues)
	    (map #'(lambda (new-row)
		     (adjoin-position new-row k rest-of-queues))
		 (enumerate-interval 1 size)))
	(queue-col (- col 1) size)))))
(defun (queens board-size)
    