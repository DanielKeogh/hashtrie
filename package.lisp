;;;; package.lisp

(defpackage #:hashtrie
  (:documentation  "Persistent/Immutable Hash Trie datastructure based upon Clojure")
  (:use #:cl)
  (:shadow
   #:length
   #:map
   #:reduce
   #:remove)
  (:export
   ;; Important functions
   #:make-hashtrie
   #:with-transient
   #:add
   #:remove
   #:value
   #:has-key
   #:map
   #:reduce
   #:length

   ;; Types
   #:hashtrie
   #:hashtrie-p
   #:persistent-hashtrie
   #:persistent-hashtrie-p
   #:transient-hashtrie
   #:transient-hashtrie-p))
