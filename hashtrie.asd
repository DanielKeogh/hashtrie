;;;; hashtrie.asd

(asdf:defsystem #:hashtrie
  :description "An implementation of the Hash Trie datastructure, based on Clojure's"
  :author "Daniel Keogh <your.name@example.com>"
  :license  "Eclipse 1.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "hashtrie")
	       (:file "api")))
