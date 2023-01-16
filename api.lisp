;;;; api.lisp

(in-package #:hashtrie)

(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(defun add (hashtrie key val)
  "Create a new map with the given key-value pair"
  (map-assoc hashtrie key val))

(defun remove (hashtrie key)
  "Remove the pair from the hash trie"
  (map-without hashtrie key))

(defun value (hashtrie key &optional not-found)
  "Get the value for a given key"
  (map-val-at hashtrie key not-found))

(defun has-key (hashtrie key)
  "Test if the key is in the hash trie"
  (let ((missing (gensym)))
    (not (equal missing (map-val-at hashtrie key missing)))))

(defun map (hashtrie fn)
  "Apply a (lambda (x y)) to each key-value pair and collect the results into a list"
  (map-map hashtrie fn))

(defun reduce (hashtrie fn &optional start-val)
  "Apply (lambda (start key val)) to aggregate all pairs of a persistent hash map"
  (map-reduce hashtrie fn start-val))

(defun length (hashtrie)
  "The number of pairs in the hash trie"
  (map-count hashtrie))

(defmacro with-transient ((name map) &body body)
  "Within the body of this macro modify a temporary, transient copy of the hash trie. e.g. (with-transient (name (make-has-trie)) <body>). The transient copy will not be thread safe.
Returns a new persistent hash trie"
  `(let ((,name (hashtrie::phm-as-transient ,map)))
     (progn ,@body)
     (hashtrie::thm-persistent ,name)))
