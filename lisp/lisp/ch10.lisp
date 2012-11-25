(= 1 1)
(/= 1 1)


(defun factorial-1 (x)
  (if (zerop x)
      1
      (* x (factorial-2 (- x 1)))))
(defun factorial-2 (x)
  (if (zerop x)
      1
      (* x (factorial-1 (- x 1)))))
