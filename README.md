# hashtrie

A fast [Hash trie](https://en.wikipedia.org/wiki/Hash_tree_(persistent_data_structure)) implementation based upon Clojure's.

A Hash Trie works like a Hash Set, except that it has been optimised for immutability and thread-safety.

By default, hash-trie's are persistent and immutable, but this implementation also supports transients for building sets significantly faster.

## Usage

Constructor:

```lisp
(htr:make-hash-trie nil "foo" 1 "bar")
;; {nil "foo", 1 "bar"}
```

Construct with transience:

```lisp
(htr:with-transient (trans (htr:make-hash-trie))
	(htr:tri-add trans 1 "bar")
	(htr:tri-add trans nil "foo")
;; {nil "foo", 1 "bar"}
```

Adding:

```lisp
(htr:tri-add (htr:make-hash-trie 1 "foo") 1 "bar")
;; {1 "bar"}
```

Removing:
```lisp
(htr:tri-remove (htr:make-hash-trie 1 1 2 2) 1)
;; {2 2}
```

Finding values:
```lisp
(htr:tri-val (htr:make-hash-trie 1 "foo" 2 "bar") 1)
;; "foo"
```

Testing keys:
```lisp
(htr:tri-has-key (htr:make-hash-trie 1 1 2 2) 1)
;; T
(htr:tri-has-key (htr:make-hash-trie 1 1 2 2) 100)
;; nil
```

Length/Count:

```lisp
(htr:tri-length (htr:make-hash-trie 1 "foo" 2 "bar"))
;; 2
```

Mapping:

```lisp
(htr:tri-map (htr:make-hash-trie 1 100 2 200 3 300)
	     (lambda (key val) (+ key val))
;; (101 202 303)
```

Reduce:

```lisp
(htr:tri-reduce (htr:make-hash-trie 1 0 2 0 3 0)
	     	(lambda (start key val) (+ start key val))
		0)
;; 6
```

## Thread Safety

In theory the persistent Hash Trie is completely thread safe. This has been tested casually, but never in a production system.

## Other important info

This library currently uses `sxhash` and `equal` for comparison. Alternative hashing/comparison functions are not supported.

## Supported Lisps

In theory should work on all Common Lisp implementations.

Has only been tested on SBCL.

## Benchmarking

Running SBCL, for comparison between using hash-trie and SBCL's own implementation of hashset, you can see that building SBCL's hashset is a bit over 10x faster. This is to be expected because it isn't immutable.

*hashtrie*

```lisp
CL-USER> (time (loop for i from 0 to 1000000
	       for map = (htr:make-hash-trie i i) then (htr:tri-add map i i)
	       finally (return map)))
;Evaluation took:
;  1.158 seconds of real time
;  1.163023 seconds of total run time (1.002477 user, 0.160546 system)
;  [ Run times consist of 0.486 seconds GC time, and 0.678 seconds non-GC time. ]
;  100.43% CPU
;  2,306,805,783 processor cycles
;  1,279,064,720 bytes consed
```

```lisp
(time (htr:with-transient (trans (htr:make-hash-trie))
	   (dotimes (i 1000000)
	     (htr:tri-add trans i i))))
;Evaluation took:
;  0.640 seconds of real time
;  0.640942 seconds of total run time (0.557056 user, 0.083886 system)
;  [ Run times consist of 0.297 seconds GC time, and 0.344 seconds non-GC time. ]
;  100.16% CPU
;  1,275,757,029 processor cycles
;  190,686,512 bytes consed
```

*hashset*

```lisp
(time (let ((m (make-hash-table :test 'equal)))
		 (dotimes (i 1000000)
		   (setf (gethash i m) i))))
; Evaluation took:
;  0.170 seconds of real time
;  0.171739 seconds of total run time (0.152880 user, 0.018859 system)
;  [ Run times consist of 0.024 seconds GC time, and 0.148 seconds non-GC time. ]
;  101.18% CPU
;  339,892,350 processor cycles
;  132,035,232 bytes consed
```
  
Also comparing the performance to clojure. Building a hash trie in sbcl is still slightly slower than doing the same in clojure, but not by much.

```clojure
;Clojure 1.10.2
(defn persistent-build-map [set n]
     (if (> n 0)
     	 (recur (assoc set n n) (dec n))
	 set))

(defn transient-build-map [n]
      (loop [i 0 v (transient {})]
      	    (if (< i n)
      	    (recur (inc i) (assoc! v i i))
      	    (persistent! v))))

(time (count (persistent-build-map {} 1000000)))
;"Elapsed time: 606.96371 msecs"

(time (count (transient-build-map 1000000)))
"Elapsed time: 502.676784 msecs"
```
