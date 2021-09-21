;;;; hashtrie-tests.asd

(asdf:defsystem #:hashtrie-tests
  :description "Tests for hashtrie"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :depends-on (:hashtrie :fiveam)
  :components ((:module "tests"
		:serial t
		:components ((:file "package")
			     (:file "main")))))
