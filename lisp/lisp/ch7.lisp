(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(dolist (x '(1 2 3)) (print x))

(defun f10 ()
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= n 10) cur)))

(defun seq10 ()
  (do ((nums nil) (i 1 (1+ i)))
      ((> i 10) (nreverse nums))
    (push i nums)))
(defun seqa10 ()
  (loop for i from 1 to 10 collecting i))