(defun abc ()
  (let ((a 1) (b 2) (c 3))
    (list a b c)))

(defun f2 ()
  (dotimes (x 10)
    (format t "~a~%" x)))

(defun f3 ()
  (let* ((x 10)
	 (y (+ x 1)))
    (list x y)))

(defvar *count* 0
  "counts the number of objects")
(defparameter *gap* 20
  "the gap of the valley")

(defvar *x* 10)
(defun foo ()
  (format t "X:~d ~%" *x*))

(defun foo1 (x)
  (setf x 10))

(setf x 1 y 2)

(defvar x 0)
(defvar y 0)


(setf x 10)
(setf (aref a 0) 10)
(set (gethash 'sss hash) 10)
(set (filed 0) 0)




(defun integer (m n)
  (if (= m n)
      (print "here")
      (cons m (integer (+ m 1) n))))

;(defun ones ()
;  (cons 1 (funcall #'ones)))