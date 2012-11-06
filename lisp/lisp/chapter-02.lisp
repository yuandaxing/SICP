(defun hello-world () (format t "hello world"))

(defun hello ()
  (format t "hello world"))
(defvar my-list (list :a 1 :b 2 :c 3))
(getf my-list :a)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "Rose" "Kathy" 7 t)

(defvar *db* nil)
(defun add-record (record)
  (push record *db*))

(add-record (make-cd "Roses" "kkkkkkkkkkkk" 7 t))
(add-record (make-cd "aaaa" "bbbbbbbb" 8 t))
(add-record (make-cd  "ttt"   "ccccc"  9 t))
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun dump-db. ()
  (format t "~{~{~a: ~10t~a~%~}~%~}" *db*))

