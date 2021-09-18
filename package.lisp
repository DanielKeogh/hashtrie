;;;; package.lisp

(defpackage #:hashtrie
  (:documentation "A fast implementation of the Hash Trie data structure, based on Clojure.")
  (:use #:cl)
  (:nicknames #:htr)
  (:export
   :make-hash-trie
   :with-transient
   :tri-add
   :tri-remove
   :tri-val
   :tri-has-key
   :tri-map
   :tri-length
   :*max-print-length*

   ;; Types
   :hash-trie
   :hash-trie-p
   :persistent-hash-map
   :persistent-hash-map-p
   :transient-hash-map
   :transient-hash-map-p))
