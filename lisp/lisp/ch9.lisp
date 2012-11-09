(defun report-result (result form)
  (format t "~:[fialed~;pass~] ... ~a~%" result form))

(defmacro check (form)
  `(report-result ,form ',form))

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))
(defun report-result (result form)
  (format t "~:[fialed~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro combine-result (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check1 (&body forms)
  `(combine-result
     ,@(loop for f in forms collect `(report-result ,f ',f))))


(defun test+ ()
  (check1
    (= (+ 1 2) 3)
    (= (+ 3 4) 5)
    (= (- 3 4) 1)))


(defvar *test-name* nil)
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defun report-result (result form)
  (format t "~:[fialed~;pass~] ...~a: ~a~%" result *test-name* form)
  result)


(deftest test+ ()
  (check1
    (= (+ 1 2) 3)
    (= (+ 3 4) 5)
    (= (- 3 4) 1)))
(deftest test- ()
  (check1 
    (= (- 1 2) 1)
    (= (- 2 3) 1)
    (= (- 3 2) 1)))

(deftest test-math ()
  (combine-result
    (test-)
    (test+)))