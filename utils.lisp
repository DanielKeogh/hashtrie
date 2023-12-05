;;;; utils.lisp

(in-package #:hashtrie)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype uint () '(unsigned-byte 32))
  (declaim (type fixnum +bits+ +size+ +mask+))
  (defconstant +bits+ 5)
  (defconstant +size+ (expt 2 +bits+))
  (defconstant +mask+ (1- +size+)))

(declaim (inline make-box))
(defun make-box ()
  (declare (optimize (speed 3) (safety 0)))
  (cons nil nil))

(defmacro box-val (b) `(car ,b))

(defmacro shift-right (i)
  `(the fixnum (+ +bits+ ,i)))

(defstruct atomic-reference (val nil))

(declaim (inline hash))
(defun hash (x)
  (declare (optimize (speed 3) (safety 0)))
  (the uint (mod (sxhash x) 4294967296)))

(defmacro logandcount (n1 n2)
  `(logcount (logand ,n1 ,n2)))

(declaim (inline equiv))
(defun equiv (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (equal v1 v2))

(declaim (inline copy-simple-array))
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

(defmacro for-true-bits32 ((mask-var int) &body body)
  (let ((x (gensym)))
    `(loop for ,x of-type (unsigned-byte 32) = ,int then (logand ,x (- ,x 1))
	   for ,mask-var = (logand ,x (- ,x))
	   while (> ,mask-var 0)
	   do (progn ,@body))))

(declaim (type (simple-array (integer 0 67) 1) *mask32->position-lookup*))
(defparameter *mask32->position-lookup*
  (loop with lookup = (make-array 67 :element-type '(integer 0 67) :initial-element 0)
	for m from 0 below 64
	for i = 1 then (ash i 1)
	do (setf (aref lookup (mod i 67)) m)
	finally (return lookup)))

(declaim (inline mask32->position))
(defun mask32->position (mask)
  (declare (optimize (speed 3) (safety 0))
	   (type (unsigned-byte 32) mask)
	   (type (simple-array (integer 0 67) 1) *mask32->position-lookup*))
  (aref *mask32->position-lookup* (mod mask 67)))
