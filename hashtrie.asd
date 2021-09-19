;;;; hashtrie.asd

(asdf:defsystem #:hashtrie
  :description "An implementation of the Hash Trie datastructure, based on Clojure's"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "hashtrie")
	       (:file "api")))
