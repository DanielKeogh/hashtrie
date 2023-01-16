;;;; tests/main.lisp

(in-package #:hashtrie-tests)

(def-suite all-tests
  :description "Main test suite for hashtrie")

(in-suite all-tests)

(defun test-hashtrie ()
  (run! 'all-tests))

(test make-hashtrie
  (is (htr:hashtrie-p (htr:make-hashtrie)))
  (is (htr:hashtrie-p (htr:make-hashtrie 1 1)))
  (is (htr:hashtrie-p (htr:make-hashtrie "one" "one")))
  (is (htr:hashtrie-p (htr:make-hashtrie nil "one"))))

(test value
  (is (= 1 (htr:value (htr:make-hashtrie nil 1) nil)))
  (is (= 1 (htr:value (htr:make-hashtrie 1 1) 1)))
  (is (= 1 (htr:value (htr:make-hashtrie "boo" 1) "boo")))
  (is (null (htr:value (htr:make-hashtrie "noo" 1) "boo"))))

(test add
  (is (= 1 (htr:length (htr:add (htr:make-hashtrie) 1 1))))
  (is (= 2 (htr:length (htr:add (htr:make-hashtrie 1 2) 2 2))))
  (is (= 1 (htr:length (htr:add (htr:make-hashtrie 1 1) 1 2))))
  (is (= 1 (htr:length (htr:add (htr:make-hashtrie 1 1) 1 2))))
  (is (= 1 (htr:length (htr:add (htr:make-hashtrie) nil 2))))
  (is (= 1000 (htr:length (loop with map = (htr:make-hashtrie)
				    for i from 1 to 1000
				    do (setf map (htr:add map i i))
				    finally (return map))))))

(test remove
  (is (= 0 (htr:length (htr:remove (htr:make-hashtrie nil 1) nil))))
  (is (= 0 (htr:length (htr:remove (htr:make-hashtrie 1 1) 1))))
  (is (= 1 (htr:length (htr:remove (htr:make-hashtrie nil 1) 1)))))

(test has-key
  (let ((map (htr:make-hashtrie)))
    (dotimes (i 1000)
      (setf map (htr:add map i i)))
    (is (loop for i from 0 below 1000
	      always (htr:has-key map i)))))

(test has-key-not
  (let ((map (htr:make-hashtrie)))
    (dotimes (i 1000)
      (setf map (htr:add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (htr:value map i))))))

(test value
  (let ((map (htr:make-hashtrie)))
    (dotimes (i 1000)
      (setf map (htr:add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (htr:value map i))
	      do (setf map (htr:remove map i))
	      never (htr:value map i)))))

(test persistent
  (let* ((map1 (htr:make-hashtrie 1 1 2 2 3 3))
	 (map2 (htr:remove map1 3))
	 (map3 (htr:add map1 4 4)))

    (is (htr:has-key map1 1))
    (is (htr:has-key map2 1))
    (is (htr:has-key map3 1))

    (is (htr:has-key map1 2))
    (is (htr:has-key map2 2))
    (is (htr:has-key map3 2))

    (is (htr:has-key map1 3))
    (is (null (htr:has-key map2 3)))
    (is (htr:has-key map3 3))

    (is (null (htr:has-key map1 4)))
    (is (null (htr:has-key map2 4)))
    (is (htr:has-key map3 4))))

(test add-remove-1000
  (let ((map (htr:make-hashtrie)))
    (dotimes (i 1000)
      (setf map (htr:add map i i)))
    (is (= 1000 (htr:length map)))
    (dotimes (i 1000)
      (setf map (htr:remove map i)))
    (is (= 0 (htr:length map)))))

(test map
  (let ((map (htr:make-hashtrie)))
    (dotimes (i 1000)
      (setf map (htr:add map i i)))))

(test with-transient-10
  (let ((map (htr:with-transient (trans (htr:make-hashtrie))
	       (dotimes (i 10)
		 (htr:add trans i i)))))
    (is (= 10 (htr:length map)))
    (let ((map2 (htr:with-transient (trans map)
		  (dotimes (i 10)
		    (htr:remove trans i)))))
      (is (= 10 (htr:length map)))  
      (is (= 0 (htr:length map2))))))

(test with-transient-1000
  (let ((map (htr:with-transient (trans (htr:make-hashtrie))
	       (dotimes (i 1000)
		 (htr:add trans i i)))))
    (is (= 1000 (htr:length map)))
    (let ((map2 (htr:with-transient (trans map)
		  (dotimes (i 1000)
		    (htr:remove trans i)))))
      (is (= 1000 (htr:length map)))  
      (is (= 0 (htr:length map2))))))

(test map-10
  (let ((map (htr:with-transient (trans (htr:make-hashtrie))
	       (dotimes (i 10)
		 (htr:add trans i i)))))

    (is (= (loop for i from 0 below 10 sum i)
	 (reduce (lambda (x y) (+ x y))
		 (htr:map map (lambda (key val) (declare (ignore key)) val)))))

    (is (= (loop for i from 0 below 10 sum i)
	 (reduce (lambda (x y) (+ x y))
		 (htr:map map (lambda (key val) (declare (ignore val)) key)))))))

(test map-10-transient
  (htr:with-transient (map (htr:make-hashtrie))
    (dotimes (i 10)
      (htr:add map i i))

    (is (= (loop for i from 0 below 10 sum i)
	   (reduce (lambda (x y) (+ x y))
		   (htr:map map (lambda (key val) (declare (ignore key)) val)))))

    (is (= (loop for i from 0 below 10 sum i)
	   (reduce (lambda (x y) (+ x y))
		   (htr:map map (lambda (key val) (declare (ignore val)) key)))))))
