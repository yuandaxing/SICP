;(in-package :com.gigamonkeys.spam)
(push "/home/ydx/cl-ppcre-2.0.3/" asdf:*central-registry*)
(asdf:load-system :cl-ppcre)


(defvar *feature-database* (make-hash-table :test #'equal))
(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defparameter *max-chars* (* 10 1024))
(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun classify (text)
  "Classify the text of a message as SPAM, HAM, or UNSURE."
  (classification (score (extract-features text))))


(defclass word-feature ()
  ((word       
    :initarg :word  ;init keyword
    :accessor word
    :initform (error "Must supply :word") ;how to init
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text) ;text process library
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

(defun spam-probability (feature)
  "Basic probability that a feature with the given relative
frequencies will appear in a spam assuming spams and hams are
otherwise equally probable. One of the two frequencies must be
non-zero."
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  "Bayesian adjustment of a given probability given the number of
data points that went into it, an assumed probability, and a
weight we give that assumed probability."
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test rig

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
        (destructuring-bind (file type) (aref corpus idx)
          (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
              (classify (start-of-file file *max-chars*))
            (list 
             :file file
             :type type
             :classification classification
             :score score)))))

(defun nshuffle-vector (vector)
  "Shuffle a vector in place using Fisher-Yates algorithm." 
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  "Return a shuffled copy of vector."
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
        (subseq text 0 read)
        text))))


(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
         (ham 'correct)
         (spam 'false-positive)
         (unsure 'missed-ham)))
      (spam
       (ecase classification
         (ham 'false-negative)
         (spam 'correct)
         (unsure 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive 
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))

(defun explain-classification (file)
  (let* ((text (start-of-file file *max-chars*))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))
  
(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))

(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))
(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))
