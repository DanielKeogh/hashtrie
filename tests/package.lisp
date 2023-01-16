;;;; tests/package.lisp

(defpackage #:hashtrie-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:htr #:hashtrie))
  (:export #:run!
	   #:all-tests))
