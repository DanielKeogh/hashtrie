;;;; api.lisp

(in-package #:hashtrie)

(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(defun tri-add (hash-trie key val)
  "Create a new map with the given key-value pair."
  (map-assoc hash-trie key val))

(defun tri-remove (hash-trie key)
  "Remove the pair from the hash trie"
  (map-without hash-trie key))

(defun tri-val (hash-trie key &optional not-found)
  "Get the value for a given key"
  (map-val-at hash-trie key not-found))

(defun tri-has-key (hash-trie key)
  "Test if the key is in the hash trie"
  (let ((missing (gensym)))
    (not (equal missing (map-val-at hash-trie key missing)))))

(defun tri-map (hash-trie fn)
  "Apply a (lambda (x y)) to each key-value pair and collect the results into a list"
  (map-map hash-trie fn))

(defun tri-reduce (hash-trie fn &optional start-val)
  "Apply (lambda (start key val)) to aggregate all pairs of a persistent hash map"
  (map-reduce hash-trie fn start-val))

(defun tri-length (hash-trie)
  "The number of pairs in the hash trie"
  (map-count hash-trie))

(defmacro with-transient ((name map) &body body)
  `(let ((,name (htr::phm-as-transient ,map)))
     (progn ,@body)
     (htr::thm-persistent ,name)))
