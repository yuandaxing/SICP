(defvar *cont* #'values)
(defmacro =lambda (parms &body body) 
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))
    
(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push #'(lambda () ,c) *paths*))
                   (reverse (cdr choices)))
         ,(car choices))
      '(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
     (progn
       (if (cdr choices)
           (push #'(lambda () (cb fn (cdr choices)))
                 *paths*))
       (funcall fn (car choices)))
     (fail)))
 
(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

;baker cooper fletcher miller smith
(=defun people-dwelling ()
  (choose-bind baker '(1 2 3 4 5)
    (choose-bind cooper '(1 2 3 4 5)
      (choose-bind fletcher '(1 2 3 4 5)
	(choose-bind miller '(1 2 3 4 5)
	  (choose-bind smith '(1 2 3 4 5)
	    (=values baker cooper fletcher miller smith)))))))


(=defun calculate ()
  (=bind (baker cooper fletcher miller smith)
      (people-dwelling)
    (if (and  
	 (distinct?  (list baker cooper fletcher miller smith))
	 (not (= baker 5))
	 (not (= cooper 1))
	 (not (= fletcher 5))
	 (not (= fletcher 1))
	 (> miller cooper)
	 (not (= (abs (- smith fletcher)) 1))
	 (not (= (abs (- fletcher cooper)) 1)))

	 (list (list 'baker baker) (list 'cooper cooper)
	       (list 'fletcher fletcher) (list 'miller miller)
	       (list 'smith smith))

	 (fail))))

(defmacro var-choose-choices (choices (&rest choosers) &rest body)
  (if (null choosers) 
       `(progn ,@body)
       `(choose-bind ,(car choosers) ,choices
	  (var-choose-choices ,choices ,(cdr choosers) ,@body))))
	 
(=defun people-dwelling-1 ()
  (var-choose-choices '(1 2 3 4 5) 
		      (baker cooper fletcher miller smith) 
		      (=values baker cooper fletcher miller smith)))
(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym
	       ,sym
	       (orb ,@(cdr args)))))))

;baker cooper fletcher miller smith
;(=defun people-dwelling ()		
;  (choose-bind baker '(1 2 3 4 5)
;    (choose-bind cooper '(1 2 3 4 5)
;      (choose-bind fletcher '(1 2 3 4 5)
;	(choose-bind miller '(1 2 3 4 5)
;	  (choose-bind smith '(1 2 3 4 5)
;	    (=values baker cooper fletcher miller smith)))))))
(defmacro require-c (name expr)
  `(if (not ,expr)
       (return-from ,name (fail))))

(defmacro list-result-requirement(result &rest requirements)
  (let ((blockname (gensym)))
    `(block ,blockname
       ,@(mapcar #'(lambda (x) `(require-c ,blockname ,x))
		 requirements)
       ,result)))
(defmacro list-entry (val)
  `(list ',val ,val))
(defmacro list-all-entries (&rest entries)
  `(list ,@(mapcar #'(lambda (entry) `(list-entry ,entry)) entries)))
(=defun house-reasoning () 
  (=bind (baker cooper fletcher miller smith) (people-dwelling-1)
    (list-result-requirement 
     (list-all-entries baker cooper fletcher miller smith)
     (distinct? (list baker cooper fletcher miller smith))
     (not (= baker 5))
     (not (= cooper 1))
     (not (= fletcher 5))
     (not (= fletcher 1))
     (> miller cooper)
     (not (= (abs (- smith fletcher)) 1))
     (not (= (abs (- fletcher cooper)) 1)))))

;why there is an error
;1 2 3 4 5 represents
;		      house no 1 2 3 4 5 
;                     red white green yellow blue
;                     tea beer water milk coffee
;                     pallmall dunhill blends bluemaster prince
;                     fish bird cat horse dog 

(=defun test()
  (=bind (baker cooper fletcher miller smith)
      (people-dwelling)
    (progn
      (if (not (= baker 5)) (fail))
      (values (list (list 'baker baker) (list 'cooper cooper)
		    (list 'fletcher fletcher) (list 'miller miller)
		    (list 'smith smith))))))

(defun distinct? (items)
  (cond ((null items) t)
	((member (car items) (cdr items)) nil)
	(t (distinct? (cdr items)))))

(defmacro =require (expr)
  `(if (not ,expr) (fail)))

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))
(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
	`(the sum of ,n1 ,n2)
	(fail))))

(=defun people-character ()
  (var-choose-choices '(1 2 3 4 5) 
		      (ep sp dp np gp)
		      (=values ep sp dp np gp)))
(=defun Einstein-test ()
  (=bind (eno sno dno nno gno) (people-character)
    (if (distinct? (list eno sno dno nno gno))
	(=bind (ec sc dc nc gc) (people-character)
	  (if (distinct? (list ec sc dc nc gc))
	      (let ((nos (list eno sno dno nno gno))
		    (cs  (list ec sc dc nc gc)))
		(if (and 
		     (= eno 2)
		     (= dno gc))
		    (progn
		      (format t "~A~%" gc)
		      (list eno  sno  dno nno gno ec sc dc nc gc))
		    (fail))))))))

(=defun people-character-1 ()
  (var-choose-choices '(1 2 3 4 5) 
		      (eno sno dno nno gno
			   ecolor scolor dcolor ncolor gcolor
			   edrink sdrink ddrink ndrink gdrink
			   esmoke ssomke dsmoke nsmoke gsmoke
			   epat spat dpat npat gpat)
		      (=values eno sno dno nno gno
			       ecolor scolor dcolor ncolor gcolor
			       edrink sdrink ddrink ndrink gdrink
			       esmoke ssomke dsmoke nsmoke gsmoke
			       epat spat dpat npat gpat)))
(=defun Einstein-Reasoning ()
  (=bind (eno sno dno nno gno
	      ecolor scolor dcolor ncolor gcolor
	      edrink sdrink ddrink ndrink gdrink
	      esmoke ssomke dsmoke nsmoke gsmoke
	      epat spat dpat npat gpat)
      (people-character-1)
    (let ((houses (list eno    sno    dno    nno    gno))
	  (colors (list ecolor scolor dcolor ncolor gcolor))
	  (drinks (list edrink sdrink ddrink ndrink gdrink))
	  (smokes (list esmoke ssomke dsmoke nsmoke gsmoke))
	  (pats   (list epat   spat   dpat   npat   gpat)))
;      (format t "~A ~A ~A ~A ~A~%" eno sno dno nno gno)
;      (format t "~A ~A ~A ~A ~A~%" ecolor scolor dcolor ncolor  gcolor)
;      (format t "~A ~A ~A ~A ~A~%" edrink sdrink ddrink ndrink gdrink)
;      (format t "~A ~A ~A ~A ~A~%" esmoke ssomke dsmoke nsmoke gsmoke)
;      (format t "~A ~A ~A ~A ~A~%~%" epat   spat   dpat   npat   gpat)
      
      (if (and (distinct? houses) (distinct? colors) (distinct? drinks) (distinct? smokes) (distinct? pats)
	       (= ecolor 1)
	       (= spat   1)
	       (= ddrink 1) 
	       (< (get-another-property 3 colors houses)
		  (get-another-property 2 colors houses))
	       (= (get-another-property 3 colors drinks) 2)
	       (= (get-another-property 1 smokes pats) 2)
	       (= (get-another-property 4 colors smokes) 2)
	       (= (get-another-property 3 houses drinks) 3)
	       (= nno 1)
	       (= 1 (abs (- (get-another-property 3 smokes houses)
			    (get-another-property 3 pats   houses))))
	       (= 1 (abs (- (get-another-property 4 pats   houses)
			    (get-another-property 2 smokes houses))))
	       (= (get-another-property 4 smokes drinks) 4)
	       (= gsmoke 5)
	       (= (abs (- nno (get-another-property 5 colors houses)))
		  1)
	       (= 1 (abs (- (get-another-property 3 smokes houses)
			    (get-another-property 5 drinks houses)))))
	  (list houses colors drinks smokes pats)
	  (fail)))))
    

(defun get-another-property (key list1 list2)
  (cond ((null list1) (error "what's the fuck"))
	((eq key (car list1)) (car list2))
	(t (get-another-property key (cdr list1) (cdr list2)))))




(defvar *saved* nil)

(=defun re-start ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))

(=defun dft-node (tree)
  (cond ((null tree) (re-start))
	((atom tree) (=values tree))
	(t (push #'(lambda () (dft-node (cdr tree)))
		 *saved*)
	   (dft-node (car tree)))))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
	  (t (princ node)
	     (re-start)))))
