# hashtrie

A fast [Hash trie](https://en.wikipedia.org/wiki/Hash_tree_(persistent_data_structure)) implementation based upon Clojure's.

A Hash Trie works like a Hash Set, except that it has been optimised for immutability.

By default, hash-trie's are persistent and immutable, but this implementation also supports transients for building sets slightly faster.

## Usage

Constructor:

```lisp
(htr:make-hash-trie nil "foo" 1 "bar")
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
Evaluation took:
  1.158 seconds of real time
  1.163023 seconds of total run time (1.002477 user, 0.160546 system)
  [ Run times consist of 0.486 seconds GC time, and 0.678 seconds non-GC time. ]
  100.43% CPU
  2,306,805,783 processor cycles
  1,279,064,720 bytes consed
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
  
Also comparing the performance to clojure, this implementation is worse still. Not sure why yet, but I have time on my side.

```clojure
Clojure 1.10.2
       (defn init-set [set n] (if (> n 0) (recur (assoc set n n) (dec n)) set))
#'user/init-set
(time (count (init-set {} 1000000)))
"Elapsed time: 606.96371 msecs"
1000000
user=> 
```