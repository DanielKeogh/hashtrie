;;;; tests/main.lisp

(in-package #:hashtrie-tests)

(def-suite all-tests
  :description "Main test suite for hashtrie")

(in-suite all-tests)

(defun test-hashtrie ()
  (run! 'all-tests))

(test make-hash-trie
  (is (htr:hash-trie-p (htr:make-hash-trie)))
  (is (htr:hash-trie-p (htr:make-hash-trie 1 1)))
  (is (htr:hash-trie-p (htr:make-hash-trie "one" "one")))
  (is (htr:hash-trie-p (htr:make-hash-trie nil "one"))))

(test tri-val
  (is (= 1 (htr:tri-val (htr:make-hash-trie nil 1) nil)))
  (is (= 1 (htr:tri-val (htr:make-hash-trie 1 1) 1)))
  (is (= 1 (htr:tri-val (htr:make-hash-trie "boo" 1) "boo")))
  (is (null (htr:tri-val (htr:make-hash-trie "noo" 1) "boo"))))

(test tri-add
  (is (= 1 (htr:tri-length (htr:tri-add (htr:make-hash-trie) 1 1))))
  (is (= 2 (htr:tri-length (htr:tri-add (htr:make-hash-trie 1 2) 2 2))))
  (is (= 1 (htr:tri-length (htr:tri-add (htr:make-hash-trie 1 1) 1 2))))
  (is (= 1 (htr:tri-length (htr:tri-add (htr:make-hash-trie 1 1) 1 2))))
  (is (= 1 (htr:tri-length (htr:tri-add (htr:make-hash-trie) nil 2))))
  (is (= 1000 (htr:tri-length (loop with map = (htr:make-hash-trie)
				    for i from 1 to 1000
				    do (setf map (htr:tri-add map i i))
				    finally (return map))))))

(test tri-remove
  (is (= 0 (htr:tri-length (htr:tri-remove (htr:make-hash-trie nil 1) nil))))
  (is (= 0 (htr:tri-length (htr:tri-remove (htr:make-hash-trie 1 1) 1))))
  (is (= 1 (htr:tri-length (htr:tri-remove (htr:make-hash-trie nil 1) 1)))))

(test tri-has-key
  (let ((map (htr:make-hash-trie)))
    (dotimes (i 1000)
      (setf map (htr:tri-add map i i)))
    (is (loop for i from 0 below 1000
	      always (htr:tri-has-key map i)))))

(test tri-has-key-not
  (let ((map (htr:make-hash-trie)))
    (dotimes (i 1000)
      (setf map (htr:tri-add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (htr:tri-val map i))))))

(test tri-val
  (let ((map (htr:make-hash-trie)))
    (dotimes (i 1000)
      (setf map (htr:tri-add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (htr:tri-val map i))
	      do (setf map (htr:tri-remove map i))
	      never (htr:tri-val map i)))))

(test persistent
  (let* ((map1 (htr:make-hash-trie 1 1 2 2 3 3))
	 (map2 (htr:tri-remove map1 3))
	 (map3 (htr:tri-add map1 4 4)))

    (is (htr:tri-has-key map1 1))
    (is (htr:tri-has-key map2 1))
    (is (htr:tri-has-key map3 1))

    (is (htr:tri-has-key map1 2))
    (is (htr:tri-has-key map2 2))
    (is (htr:tri-has-key map3 2))

    (is (htr:tri-has-key map1 3))
    (is (null (htr:tri-has-key map2 3)))
    (is (htr:tri-has-key map3 3))

    (is (null (htr:tri-has-key map1 4)))
    (is (null (htr:tri-has-key map2 4)))
    (is (htr:tri-has-key map3 4))))

(test add-remove-1000
  (let ((map (htr:make-hash-trie)))
    (dotimes (i 1000)
      (setf map (htr:tri-add map i i)))
    (is (= 1000 (htr:tri-length map)))
    (dotimes (i 1000)
      (setf map (htr:tri-remove map i)))
    (is (= 0 (htr:tri-length map)))))
