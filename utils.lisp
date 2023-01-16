;;;; utils.lisp

(in-package #:hashtrie)

(defconstant +bits+ 5)
(defconstant +size+ (expt 2 +bits+))
(defconstant +mask+ (1- +size+))

(defun make-box ()
  (declare (optimize (speed 3) (safety 0)))
  (cons nil nil))

(defmacro box-val (b) `(car ,b))

(defmacro shift-right (i)
  `(the fixnum (+ +bits+ ,i)))

(defstruct atomic-reference (val nil))

(defun hash (x)
  (declare (optimize (speed 3) (safety 0)))
   (sxhash x))

(defmacro logandcount (n1 n2)
  `(logcount (logand ,n1 ,n2)))

(defun equiv (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (equal v1 v2))

(defun copy-simple-array (arr)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t (*)) arr))
  (copy-seq arr))

(defun array-copy (src src-pos dest dest-start length)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t (*)) src dest)
	   (type fixnum src-pos dest-start length))
  (loop for cnt fixnum from 0 below length
	for src-index fixnum from src-pos
	for dest-index fixnum from dest-start
	do (setf (aref dest dest-index) (aref src src-index))))

(defun remove-pair (array i)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t (*)) array)
	   (type fixnum i))
  (let ((new-array (make-array (- (cl:length array) 2))))
    (array-copy array 0 new-array 0 (the fixnum (* 2 i)))
    (array-copy array (the fixnum (* 2 (1+ i))) new-array
		(the fixnum (* 2 i))
		(the fixnum (- (cl:length new-array) (* 2 i))))
    new-array))
