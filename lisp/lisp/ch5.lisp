(defun foo (a b &optional c d)
  (list a b c d))
(defun foo1 (a b &optional (c 10) (d 20))
  (list a b c d))
(defun foo (a b &optional (c 10 c-supplied-p) d)
  (list a b c d c-supplied-p))

(defun f1 (a &rest r)
  r)

(defun foo2 (&key a b c)
  (list a b c))

(defun foo3 (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
	(return-from foo3 (list i j))))))

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))