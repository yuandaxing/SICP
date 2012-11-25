(defun read-u2 (in)
  (+ (* (read-byte in) 256) (read-byte in)))
(defun read-u2-a (in)
  (let ((a 0))
    (setf (ldb (byte 8 8) a) (read-byte in))
    (setf (ldb (byte 8 0) a) (read-byte in))
    a))

(defun write-u2 (out v)
  (write-byte (ldb (byte 8 8) v) out)
  (write-byte (ldb (byte 8 0) v) out))

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
	 until (char = char +null+) do (write-char char s))))

(defun write-null-terminated-ascii (str out)
  (loop for char across str
       do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

(defclass id3-tag ()
  ((identifier :initarg :identifier :accessor identifier)
   (major-version :initarg :major-version :accessor major-version)
   (revision :initarg :revision :accessor revision)
   (flags :initarg :flags :accessor flags)
   (size :initarg :size  :accessor size)
   (frames :initarg :frames :accessor frames)))

(defun read-id3-tag (in)
  (let ((tag (make-instance 'id3-tag)))
    (with-slots (identifier major-version revision flags size frames) tag
      (setf identifier  (read-iso-8859-1-string in :length 3))
      (setf major-version (read-u1 in))
      (setf revision (read-u1 in))
      (setf flags    (read-u1 in))
      (setf size     (read-id3-encoded-size in))
      (setf frames   (read-id3-frames in :tag-size size)))
    tag))


