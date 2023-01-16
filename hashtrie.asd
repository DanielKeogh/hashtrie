;;;; hashtrie.asd

(asdf:defsystem #:hashtrie
  :description "Persistent/Immutable Hash Trie datastructure based upon Clojure"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "hashtrie")
	       (:file "api")))
