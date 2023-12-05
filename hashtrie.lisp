;;;; hashtrie.lisp

(in-package #:hashtrie)

(defstruct (hashtrie (:constructor nil)))
(defstruct (persistent-hashtrie (:conc-name phm-)
				(:include hashtrie))
  (meta nil :type (or null hashtrie))
  (count nil :type uint :read-only t)
  (root nil :type (or null hash-map-node) :read-only t)
  (has-null nil :type boolean :read-only t)
  (null-value nil :read-only t))

(defstruct (transient-hashtrie (:conc-name thm-)
			       (:include hashtrie))
  (edit nil :read-only t)
  (count nil :type uint)
  (root nil :type (or null hash-map-node))
  (has-null nil :type boolean)
  (null-value nil)
  (leaf-flag (make-box) :read-only t))

(defstruct hash-map-node)

(defstruct (hash-map-bitmap-node (:conc-name hmn-)
				 (:include hash-map-node))
  (edit nil)
  (bitmap nil :type uint)
  (array nil :type (simple-array t (*))))

(defstruct (hash-map-array-node (:conc-name hman-)
				(:include hash-map-node))
  (edit nil)
  (count nil :type uint)
  (array nil :type (simple-array t (*))))

(defstruct (hash-map-collision-node (:conc-name hmcn-)
				    (:include hash-map-node))
  (edit nil)
  (hash nil :type uint :read-only t)
  (count nil :type uint)
  (array nil :type (simple-array t (*))))

(defgeneric map-make-iterator (hash-map))
(defgeneric map-val-at (hash-map key &optional not-found))
(defgeneric map-without (hash-map key))
(defgeneric map-count (hash-map))
(defgeneric node-make-iterator (node))
(defgeneric node-find (node shift hash key not-found))
(defgeneric node-without (node shift hash key))
(defgeneric node-without-edit (node edit shift hash key removed-leaf))

(defvar *empty-hash-iterator* (lambda () (values nil nil nil)))
(declaim (type hash-map-bitmap-node *empty-hash-map-node*))
(defvar *empty-hash-map-node* (make-hash-map-bitmap-node :bitmap 0 :array (make-array 0)))
(declaim (type persistent-hashtrie *empty-hash-map*))
(defvar *empty-hash-map* (make-persistent-hashtrie :count 0 :root nil :has-null nil :null-value nil))
(defvar *not-found* (gensym))

(declaim (inline hmn-index))
(defun hmn-index (node bit)
  (declare (optimize (speed 3) (safety 0))
	   (type uint bit))
  (logandcount (hmn-bitmap node) (1- bit)))

(declaim (inline mask))
(defun mask (hash shift)
  (declare (type uint hash)
	   (type fixnum shift)
	   (optimize (speed 3) (safety 0)))
  (the uint (logand (ash hash (the fixnum (- shift))) +mask+)))

(declaim (inline bitpos))
(defun bitpos (hash shift)
  (declare (type uint hash)
	   (type fixnum shift)
	   (optimize (speed 3) (safety 0)))
  (the uint (ash 1 (the uint (mask hash shift)))))

(declaim (inline clone-and-set1))
(defun clone-and-set1 (array i new-val)
  (declare (type (simple-array t (*)) array)
	   (optimize (speed 3) (safety 0)))
  (let ((new-array (copy-seq array)))
    (declare (type (simple-array t (*)) new-array))
    (setf (aref new-array i) new-val)
    new-array))

(defun clone-and-set2 (array i new-val j new-val2)
  (declare (type (simple-array t (*)) array)
	   (optimize (speed 3) (safety 0)))
  (let ((new-array (copy-seq array)))
    (declare (type (simple-array t (*)) new-array))
    (setf (aref new-array i) new-val)
    (setf (aref new-array j) new-val2)
    new-array))

(declaim (inline node-assoc-edit))
(defun node-assoc-edit (node edit shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0)))
  (typecase node
    (hash-map-array-node (node-assoc-edit-array-node node edit shift hash key val added-leaf))
    (hash-map-bitmap-node (node-assoc-edit-bitmap-node node edit shift hash key val added-leaf))
    (hash-map-collision-node (node-assoc-edit-collision-node node edit shift hash key val added-leaf))))

(declaim (inline node-assoc))
(defun node-assoc (node shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0)))
  (typecase node
    (hash-map-array-node (node-assoc-array-node node shift hash key val added-leaf))
    (hash-map-bitmap-node (node-assoc-bitmap-node node shift hash key val added-leaf))
    (hash-map-collision-node (node-assoc-collision-node node shift hash key val added-leaf))))

(defun create-node (shift key1 val1 key2hash key2 val2)
  (declare (optimize (speed 3) (safety 0))
	   (type uint key2hash))
  (let ((key1hash (the uint (hash key1))))
    (if (= key1hash key2hash)
	(make-hash-map-collision-node :edit nil :hash key1hash :count 2 :array (make-array 4 :initial-contents (list key1 val1 key2 val2)))
	(let* ((added-leaf (make-box))
	       (edit (make-atomic-reference))
	       (n1 (node-assoc-edit-bitmap-node *empty-hash-map-node* edit shift key1hash key1 val1 added-leaf)))
	  (node-assoc-edit n1 edit shift key2hash key2 val2 added-leaf)))))

(defun create-edit-node (edit shift key1 val1 key2hash key2 val2)
  (declare (optimize (speed 3) (safety 0))
	   (type uint key2hash))
  (let ((key1hash (hash key1)))
    (if (= key1hash key2hash)
	(make-hash-map-collision-node :edit nil :hash key1hash :count 2 :array (make-array 0 :initial-contents (list key1 val1 key2 val2)))
	(let* ((added-leaf (make-box))
	       (n1 (node-assoc-edit-bitmap-node *empty-hash-map-node* edit shift key1hash key1 val1 added-leaf)))
	  (node-assoc-edit n1 edit shift key2hash key2 val2 added-leaf)))))

;;; Persisent hash map impl

(defun phm-as-transient (map)
  (declare (optimize (speed 3) (safety 0)))
  (make-transient-hashtrie
   :edit (make-atomic-reference :val t)
   :count (phm-count map)
   :root (phm-root map)
   :has-null (phm-has-null map)
   :null-value (phm-null-value map)))

(defun persistent-map-assoc (map key val)
  (declare (optimize (speed 3) (safety 0))
	   (type persistent-hashtrie map))
  (with-accessors ((has-null phm-has-null)
		   (null-value phm-null-value)
		   (count phm-count)
		   (root phm-root)
		   (meta phm-meta))
      map
    (if (not key)
	(if (and has-null (eq val null-value))
	    map
	    (make-persistent-hashtrie :meta meta
				      :count (if has-null count (1+ count))
				      :root root
				      :has-null t
				      :null-value val))
	(let* ((added-leaf (make-box))
	       (new-root (node-assoc (if (null root) *empty-hash-map-node* root)
				     0 (hash key) key val added-leaf)))
	  (if (eq new-root root)
	      map
	      (make-persistent-hashtrie :meta meta
					:count (if (box-val added-leaf)
						   (1+ count)
						   count)
					:root new-root
					:has-null has-null
					:null-value null-value))))))

(defmethod map-val-at ((map persistent-hashtrie) key &optional not-found)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((root phm-root)
		   (has-null phm-has-null)
		   (null-value phm-null-value))
      map
    (if (not key)
	(if has-null null-value not-found)
	(if root (node-find root 0 (hash key) key not-found) not-found))))

(defmethod map-make-iterator ((map persistent-hashtrie))
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((root phm-root)
		   (has-null phm-has-null)
		   (null-value phm-null-value))
      map
    (let ((itr (if root (node-make-iterator root)
		   *empty-hash-iterator*)))
      (declare (type (function ()) itr))
      (if has-null
	  (let (returned-nil)
	    (lambda ()
	      (if returned-nil
		  (funcall itr)
		  (progn
		    (setf returned-nil t)
		    (values t nil null-value)))))
	  itr))))

(defmethod map-without ((map persistent-hashtrie) key)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((meta phm-meta)
		   (count phm-count)
		   (root phm-root)
		   (has-null phm-has-null))
      map
    (cond ((null key) (if (phm-has-null map)
			  (make-persistent-hashtrie :meta meta
						    :count (the uint (1- count))
						    :root root
						    :has-null nil
						    :null-value nil)))
	  ((null root) map)
	  (t (let ((new-root (node-without root 0 (hash key) key)))
	       (if (eq root new-root)
		   map
		   (make-persistent-hashtrie :meta meta
					     :count (the uint (1- count))
					     :root new-root
					     :has-null has-null
					     :null-value nil)))))))

(defmethod map-count ((map persistent-hashtrie))
  (declare (optimize (speed 3) (safety 0)))
  (phm-count map))

;;; transient-hashtrie impl

(defun thm-do-assoc (map key val)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((has-null thm-has-null)
		   (null-value thm-null-value)
		   (count thm-count)
		   (leaf-flag thm-leaf-flag)
		   (edit thm-edit)
		   (root thm-root))
      map
    (if (not key)
	(progn (setf null-value val)
	       (unless has-null
		 (incf count)
		 (setf has-null t)))
	;; else
	(progn
	  (setf (box-val leaf-flag) nil)
	  (let ((n (node-assoc-edit (or root *empty-hash-map-node*)
				    edit
				    0 (hash key) key val leaf-flag)))
	    (unless (equal n root)
	      (setf root n))
	    (when (eq (box-val leaf-flag) leaf-flag) (incf count))))))
  map)

(defun thm-do-without (map key)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((has-null thm-has-null)
		   (null-value thm-null-value)
		   (count thm-count)
		   (leaf-flag thm-leaf-flag)
		   (root thm-root)
		   (edit thm-edit))
      map
    (if (not key)
	(when has-null
	  (setf has-null nil)
	  (setf null-value nil)
	  (decf count))
	(when root
	  (setf (box-val leaf-flag) nil)
	  (let ((n (node-without-edit root edit 0 (hash key) key leaf-flag)))
	    (unless (eq n root) (setf root n))
	    (when (eq (box-val leaf-flag) leaf-flag) (decf count))))))
  map)

(defun thm-do-val-at (map key not-found)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((root thm-root)
		   (null-value thm-null-value)
		   (has-null thm-has-null))
      map
    (cond ((null key)
	   (if has-null
	       null-value
	       not-found))
	  ((null root) not-found)
	  (t (node-find root 0 (hash key) key not-found)))))

(defun thm-ensure-editable (map)
  (declare (optimize (speed 3) (safety 0)))
  (unless (atomic-reference-val (thm-edit map))
    (error "Transient used after persistent call")))

(defun thm-do-persistent (map)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((count thm-count)
		   (root thm-root)
		   (has-null thm-has-null)
		   (null-value thm-null-value)
		   (edit thm-edit))
      map
    (setf (atomic-reference-val edit) nil)
    (make-persistent-hashtrie :count count :root root
			      :has-null has-null :null-value null-value)))

(defun thm-persistent (map)
  (declare (optimize (speed 3) (safety 0)))
  (thm-ensure-editable map)
  (thm-do-persistent map))

(defun transient-map-assoc (map key value)
  (declare (optimize (speed 3) (safety 0))
	   (type transient-hashtrie map))
  (thm-ensure-editable map)
  (thm-do-assoc map key value))

(defmethod map-without ((map transient-hashtrie) key)
  (declare (optimize (speed 3) (safety 0)))
  (thm-ensure-editable map)
  (thm-do-without map key))

(defmethod map-val-at ((map transient-hashtrie) key &optional not-found)
  (declare (optimize (speed 3) (safety 0)))
  (thm-ensure-editable map)
  (thm-do-val-at map key not-found))

(defmethod map-make-iterator ((map transient-hashtrie))
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((root thm-root)
		   (has-null thm-has-null)
		   (null-value thm-null-value))
      map
    (let ((itr (if root (node-make-iterator root)
		   *empty-hash-iterator*)))
      (declare (type (function ()) itr))
      (if has-null
	  (let (returned-nil)
	    (lambda ()
	      (if returned-nil
		  (progn (setf returned-nil t)
			 (values t nil null-value))
		  (funcall itr))))
	  itr))))

(defmethod map-count ((map transient-hashtrie))
  (declare (optimize (speed 3) (safety 0)))
  (thm-count map))

;;; hash-map-array-node

(defun node-assoc-array-node (this shift hash key val added-leaf)
  (declare (type fixnum shift)
	   (type uint hash)
	   (type hash-map-array-node this)
	   (optimize (speed 3) (safety 0)))
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      (declare (type uint idx)
	       (type (or null hash-map-node) node))

      (if (null node)
	  (let ((new-node (node-assoc-bitmap-node *empty-hash-map-node*
				      (shift-right shift) hash key val added-leaf)))
	    (make-hash-map-array-node
	     :edit nil
	     :count (1+ count)
	     :array (clone-and-set1 array idx new-node)))

	  (let ((n (node-assoc node (shift-right shift) hash key val added-leaf)))
	    (if (eq n node)
		this
		(make-hash-map-array-node
		 :edit nil
		 :count count
		 :array (clone-and-set1 array idx n))))))))

(declaim (inline hman-ensure-editable))
(defun hman-ensure-editable (node edit)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq (hman-edit node) edit)
      node
      (make-hash-map-array-node :edit edit
				:count (hman-count node)
				:array (copy-simple-array (hman-array node)))))

(declaim (inline hman-edit-and-set))
(defun hman-edit-and-set (node edit i n)
  (declare (optimize (speed 3) (safety 0)))
  (let ((editable (hman-ensure-editable node edit)))
    (setf (aref (hman-array editable) i) n)
    editable))

(defun node-assoc-edit-array-node (this edit shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift)
	   (type hash-map-array-node this))
  (let* ((array (hman-array this))
	 (idx (mask hash shift))
	 (node (aref array idx)))
    (declare (type uint idx)
	     (type (or null hash-map-node) node))
    (if (null node)
	(let* ((n (node-assoc-edit-bitmap-node *empty-hash-map-node* edit (shift-right shift) hash key val added-leaf))
	       (editable (hman-edit-and-set this edit idx n)))
	  (incf (hman-count editable))
	  editable)
	(let ((n (node-assoc-edit node edit (shift-right shift) hash key val added-leaf)))
	  (declare (type hash-map-node n))
	  (if (eq n node)
	      this
	      (hman-edit-and-set this edit idx n))))))

(defun hman-pack (node edit idx)
  (declare (optimize (speed 3) (safety 0))
	   (type uint idx))
  (with-accessors ((count hman-count)
		   (array hman-array))
      node
    (let ((new-array (make-array (* 2 (1- count)) :initial-element nil))
	  (j 1)
	  (bitmap 0))
      (declare (type uint bitmap))
      (dotimes (i idx)
	(declare (type uint i))
	(when (aref array i)
	  (setf (aref new-array j) (aref array i))
	  (setf bitmap (logior bitmap (the uint (ash 1 i))))
	  (incf j 2)))
      (loop for i from (1+ idx) below (cl:length array)
	    for v = (aref array i)
	    when v do
	      (setf (aref new-array j) v)
	      (setf bitmap (logior bitmap (the uint (ash 1 i))))
	      (incf j 2))
      (make-hash-map-bitmap-node :edit edit :bitmap bitmap :array new-array))))

(defmethod node-without-edit ((this hash-map-array-node) edit shift hash key removed-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift)
	   (type uint hash))
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      (if (null node)
	  this
	  (let ((n (node-without-edit node edit (shift-right shift) hash key removed-leaf)))
	    (if (eq n node)
		this
		(if (null n)
		    (if (<= count 8)
			(hman-pack this edit idx)
			(let ((editable (hman-edit-and-set this edit idx n)))
			  (decf (hman-count editable))
			  editable))
		    (hman-edit-and-set this edit idx n))))))))

(defmethod node-without ((this hash-map-array-node) shift hash key)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      (if (null node)
	  this
	  (let ((n (node-without node (shift-right shift) hash key)))
	    (cond ((eq n node) this)
		  ((null n)
		   (if (<= count 8)
		       (hman-pack this nil idx)
		       (make-hash-map-array-node
			:edit nil
			:count (1- count)
			:array (clone-and-set1 array idx n))))
		  (t (make-hash-map-array-node
		      :edit nil
		      :count count
		      :array (clone-and-set1 array idx n)))))))))

(defmethod node-find ((this hash-map-array-node) shift hash key not-found)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (let* ((idx (mask hash shift))
	 (node (aref (hman-array this) idx)))
    (if (null node)
	not-found
	(node-find node (shift-right shift) hash key not-found))))

(defmethod node-make-iterator ((node hash-map-array-node))
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((array hman-array))
      node
    (let ((i 0)
	  nested-itr)
      (declare (type uint i))
      (lambda ()
	(loop while t do
	  (cond (nested-itr
		 (multiple-value-bind (has-val key val)
		     (funcall (the (function ()) nested-itr))
		   (if has-val
		       (return (values has-val key val))
		       (progn (setf nested-itr nil)
			      (incf i)))))
		((< i (cl:length array))
		 (let ((n (aref array i)))
		   (if n
		       (setf nested-itr (node-make-iterator n))
		       (incf i))))
		(t (return (values nil nil nil)))))))))

;;; hash-map-bitmap-node

(defun hmn-ensure-editable (node edit)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((this-edit hmn-edit)
		   (bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (if (eq edit this-edit)
	node
	(let* ((n (logcount bitmap))
	       (new-array (make-array (if (>= n 0) (* 2 (1+ n)) 4) :initial-element nil)))
	  (array-copy array 0 new-array 0 (* 2 n))
	  (make-hash-map-bitmap-node :edit edit :bitmap bitmap :array new-array)))))

(defun hmn-edit-and-set (node edit i a &optional j b)
  (declare (optimize (speed 3) (safety 0)))
  (let ((editable (hmn-ensure-editable node edit)))
    (setf (aref (hmn-array editable) i) a)
    (when j (setf (aref (hmn-array editable) j) b))
    editable))

(defun hmn-edit-and-remove-pair (node edit bit i)
  (declare (optimize (speed 3) (safety 0))
	   (type uint bit i))
  (with-accessors ((bitmap hmn-bitmap))
      node
    (when (/= (the uint bitmap) bit)
      (let* ((editable (hmn-ensure-editable node edit))
	     (array (hmn-array editable))
	     (array-len (cl:length array)))
	(setf (hmn-bitmap editable) (logxor (hmn-bitmap editable) bit))
	(let* ((new-len (* 2 (the uint (1+ i))))
	       (i2 (* 2 i))
	       (remaining (- array-len new-len)))
	  (declare (type uint i2 remaining new-len))
	  (array-copy array new-len array i2 remaining))
	(setf (aref array (- array-len 2)) nil)
	(setf (aref array (- array-len 1)) nil)
	editable))))

(defun node-assoc-edit-bitmap-node (node edit shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift)
	   (type hash-map-bitmap-node node))
  (let* ((bitmap (hmn-bitmap node))
	 (array (hmn-array node))
	 (bit (bitpos hash shift))
	 (2idx (* 2 (hmn-index node bit))))
    (declare (type uint bit 2idx))
    (if (/= 0 (logand bitmap bit))
	(let ((key-or-null (aref array 2idx))
	      (value-or-node (aref array (1+ 2idx))))
	  (cond
	    ((null key-or-null)
	     (let ((n (node-assoc-edit value-or-node edit (shift-right shift) hash key val added-leaf)))
	       (if (equal n value-or-node)
		   node
		   (hmn-edit-and-set node edit (shift-right shift) n))))
	    ((equiv key key-or-null)
	     (if (equal val value-or-node)
		 node
		 (hmn-edit-and-set node edit (shift-right shift) val)))
	    (t
	     (setf (box-val added-leaf) added-leaf)
	     (hmn-edit-and-set node edit
			       2idx nil
			       (the fixnum (1+ 2idx))
			       (create-edit-node edit (shift-right shift) key-or-null value-or-node hash key val)))))
	;; else
	(let ((n (logcount bitmap)))
	  (cond
	    ((< (* 2 n) (cl:length array))
	     (setf (box-val added-leaf) added-leaf)
	     (let* ((editable (hmn-ensure-editable node edit))
		    (array (hmn-array editable)))
	       (declare (type uint 2idx))
	       (array-copy array 2idx array
			   (the uint (+ 2 2idx))
			   (the uint (- (* 2 n) 2idx)))
	       (setf (aref array 2idx) key)
	       (setf (aref array (1+ 2idx)) val)
	       (setf (hmn-bitmap editable) (logior bit (hmn-bitmap editable)))
	       editable))

	    ((>= n 16)
	     (let* ((nodes (make-array +size+ :initial-element nil))
		    (jdx (mask hash shift))
		    (j 0))
	       (setf (aref nodes jdx) (node-assoc-edit-bitmap-node *empty-hash-map-node* edit (shift-right shift) hash key val added-leaf))
	       (for-true-bits32 (mask bitmap)
		 (let ((i (mask32->position mask)))
		  (if (null (aref array j))
		      (setf (aref nodes i) (aref array (1+ j)))
		      (setf (aref nodes i) (node-assoc-edit-bitmap-node *empty-hash-map-node* edit (shift-right shift)
									(hash (aref array j)) (aref array j) (aref array (1+ j)) added-leaf)))
		   (incf j 2)))
	       (make-hash-map-array-node :edit edit :count (1+ n) :array nodes)))

	    (t
	     (let ((new-array (make-array (* 2 (1+ n)) :initial-element nil)))
	       (array-copy array 0 new-array 0 2idx)
	       (setf (aref new-array 2idx) key)
	       (setf (box-val added-leaf) added-leaf)
	       (setf (aref new-array (1+ 2idx)) val)
	       (array-copy array 2idx new-array (the uint (+ 2 2idx)) (the uint (- (* 2 n) 2idx)))

	       (if (eq edit (hmn-edit node))
		   (progn
		     (setf (hmn-array node) new-array)
		     (setf (hmn-bitmap node) (logior (hmn-bitmap node) bit))
		     node)
		   (make-hash-map-bitmap-node :edit edit :bitmap bit :array new-array)))))))))

(defun node-assoc-bitmap-node (node shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type uint hash)
	   (type fixnum shift)
	   (type hash-map-bitmap-node node))
  (let* ((bit (bitpos hash shift))
	 (idx (hmn-index node bit))
	 (bitmap (hmn-bitmap node))
	 (array (hmn-array node)))
    (declare (type uint idx bit bitmap))
    (if (/= 0 (logand bitmap bit))
	(let ((key-or-null (aref array (the uint (* 2 idx))))
	      (value-or-node (aref array (the uint (1+ (* 2 idx))))))
	  (cond
	    ((null key-or-null)
	     (let ((n (node-assoc value-or-node (shift-right shift) hash key val added-leaf)))
	       (if (equal n value-or-node)
		   node
		   (make-hash-map-bitmap-node :bitmap bitmap
					      :array (clone-and-set1 array (the uint (1+ (* 2 idx))) n)))))
	    ((equiv key key-or-null)
	     (if (equal val value-or-node)
		 node
		 (make-hash-map-bitmap-node :bitmap bitmap
					    :array (clone-and-set1 array (the uint (1+ (* 2 idx))) val))))
	    (t
	     (setf (box-val added-leaf) added-leaf)
	     (make-hash-map-bitmap-node :bitmap bitmap
					:array (clone-and-set2 array
							       (the uint (* 2 idx)) nil
							       (the uint (1+ (* 2 idx)))
							       (create-node (shift-right shift) key-or-null value-or-node hash key val))))))
	;; else
	(let ((n (logcount bitmap)))
	  (declare (type fixnum n))
	  (if (>= n 16)
	      (let* ((nodes (make-array +size+ :initial-element nil))
		     (jdx (mask hash shift))
		     (j 0))
		(declare (type uint j jdx))
		(setf (aref nodes jdx) (node-assoc-bitmap-node *empty-hash-map-node* (shift-right shift) hash key val added-leaf))
		
		(for-true-bits32 (bit bitmap)
		  (let ((i (mask32->position bit)))
		    (if (null (aref array j))
			(setf (aref nodes i) (aref array (1+ j)))
			(setf (aref nodes i) (node-assoc-bitmap-node *empty-hash-map-node* (shift-right shift) (hash (aref array j)) (aref array j) (aref array (1+ j)) added-leaf)))
		    (incf j 2)))
		(make-hash-map-array-node :count (1+ n) :array nodes))
	      ;; else
	      (let ((new-array (make-array (* 2 (1+ n)) :initial-element nil)))
		(array-copy array 0 new-array 0 (the uint (* 2 idx)))
		(setf (aref new-array (* 2 idx)) key)
		(setf (box-val added-leaf) added-leaf)
		(setf (aref new-array (1+ (* 2 idx))) val)
		(array-copy array (the uint (* 2 idx)) new-array (the uint (* 2 (1+ idx))) (the uint (* 2 (- n idx))))
		(make-hash-map-bitmap-node :bitmap (logior bitmap bit) :array new-array)))))))

(defmethod node-find ((node hash-map-bitmap-node) shift hash key not-found)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (let ((bit (bitpos hash shift)))
      (declare (type uint bit))
      (if (= (logand bitmap bit) 0)
	  not-found
	  (let* ((idx (hmn-index node bit))
		 (2idx (* 2 idx))
		 (key-or-null (aref array 2idx))
		 (val-or-node (aref array (1+ 2idx))))
	    (declare (type uint idx 2idx))
	    (cond ((not key-or-null) (node-find val-or-node (shift-right shift) hash key not-found))
		  ((equiv key key-or-null) val-or-node)
		  (t not-found)))))))

(defmethod node-without-edit ((this hash-map-bitmap-node) edit shift hash key removed-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      this
    (let ((bit (bitpos hash shift)))
      (declare (type uint bit))
      (if (= 0 (logand bitmap bit))
	  this
	  (let* ((idx (hmn-index this bit))
		 (2idx (* 2 idx))
		 (key-or-null (aref array 2idx))
		 (val-or-node (aref array (1+ 2idx))))
	    (declare (type uint idx 2idx)
		     (type hash-map-node val-or-node))
	    (cond
	      ((null key-or-null)
	       (let ((n (node-without-edit val-or-node edit (shift-right shift) hash key removed-leaf)))
		 (cond
		   ((eql n val-or-node) this)
		   (n (hmn-edit-and-set this edit (1+ 2idx) n))
		   ((= bitmap bit) nil)
		   (t (hmn-edit-and-remove-pair this edit bit idx)))))
	      ((equiv key key-or-null)
	       (setf (box-val removed-leaf) removed-leaf)
	       (hmn-edit-and-remove-pair this edit bit idx))
	      (t this)))))))

(defmethod node-without ((node hash-map-bitmap-node) shift hash key)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (let ((bit (bitpos hash shift)))
      (declare (type uint bit))
      (if (= 0 (logand bitmap bit))
	  node
	  (let* ((idx (hmn-index node bit))
		 (2idx (* 2 idx))
		 (key-or-null (aref array 2idx))
		 (val-or-node (aref array (1+ 2idx))))
	    (declare (type uint idx 2idx))
	    (cond ((null key-or-null)
		   (let ((n (node-without val-or-node (shift-right shift) hash key)))
		     (cond ((eq n val-or-node) node)
			   (n (make-hash-map-bitmap-node
			       :edit nil
			       :bitmap bitmap
			       :array (clone-and-set1 array (1+ 2idx) n)))
			   ((= bitmap bit) nil)
			   (t (make-hash-map-bitmap-node
			       :edit nil
			       :bitmap (logxor bitmap bit)
			       :array (remove-pair array idx))))))

		  ((equiv key key-or-null)
		   (if (= bitmap bit) nil
		       (make-hash-map-bitmap-node
			:edit nil
			:bitmap (logxor bitmap bit)
			:array (remove-pair array idx))))
		  (t node)))))))

(defun node-make-array-iterator (array)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t (*)) array))
  (let ((i 0)
	nested-itr)
    (declare (type uint i))
    (lambda ()
      (loop while t do
	(cond (nested-itr
	       (multiple-value-bind (has-val key val)
		   (funcall (the (function ()) nested-itr))
		 (if has-val
		     (return (values has-val key val))
		     (setf nested-itr nil))))
	      ((< i (cl:length array))
	       (let ((key (aref array i))
		     (val (aref array (1+ i))))
		 (incf i 2)
		 (cond (key (return (values t key val)))
		       (val (setf nested-itr (node-make-iterator val))))))
	      (t (return (values nil nil nil))))))))

(defmethod node-make-iterator ((node hash-map-bitmap-node))
  (declare (optimize (speed 3) (safety 0)))
  (node-make-array-iterator (hmn-array node)))

;; hash-collision-node

(defun hmcn-find-index (node key)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i from 0 by 2
	repeat (hmcn-count node)
	when (equiv key (aref (hmcn-array node) i))
	  do (return-from hmcn-find-index i))
  -1)

(defun node-assoc-collision-node (node shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type uint hash)
	   (type hash-map-collision-node node))
  (with-accessors ((this-hash hmcn-hash)
		   (array hmcn-array)
		   (count hmcn-count)
		   (edit hmcn-edit))
      node
    (if (= hash this-hash)
	(let ((idx (hmcn-find-index node key)))
	  (declare (type fixnum idx))
	  (if (/= idx -1)
	      (if (equal val (aref array (1+ idx)))
		  node
		  (make-hash-map-collision-node :edit nil :hash hash :count count
						:array (clone-and-set1 array (1+ idx) val)))
	      (let ((new-array (make-array (* 2 (1+ count)) :initial-element nil)))
		(array-copy array 0 new-array 0 (the uint (* 2 count)))
		(setf (aref new-array (* 2 count)) key)
		(setf (aref new-array (1+ (* 2 count))) val)
		(setf (box-val added-leaf) added-leaf)
		(make-hash-map-collision-node :edit edit :hash hash :count (1+ count) :array new-array))))

	(node-assoc (make-hash-map-bitmap-node :bitmap (bitpos this-hash shift) :array (make-array 2 :initial-contents (list nil node)))
		    shift hash key val added-leaf))))

(defmethod node-without ((node hash-map-collision-node) shift hash key)
  (declare (optimize (speed 3) (safety 0))
	   (type uint hash))
  (let ((idx (hmcn-find-index node key)))
    (declare (type fixnum idx))
    (cond ((= idx -1) node)
	  ((= (hmcn-count node) 1) nil)
	  (t (make-hash-map-collision-node :edit nil
					   :hash hash
					   :count (1- (hmcn-count node))
					   :array (remove-pair (hmcn-array node) (floor idx 2)))))))

(defun hmcn-ensure-editable (node edit)
  (declare (optimize (speed 3) (safety 0)))
  (with-accessors ((this-edit hmcn-edit)
		   (count hmcn-count)
		   (array hmcn-array)
		   (hash hmcn-hash))
      node
    (if (eq edit this-edit)
	node
	(let ((new-array (make-array (* 2 (1+ count)) :initial-element nil)))
	  (array-copy array 0 new-array 0 (the uint (* 2 count)))
	  (make-hash-map-collision-node :edit edit
					:hash hash
					:count count
					:array new-array)))))

(defun hmcn-edit-and-set (node edit i a &optional j b)
  (declare (optimize (speed 3) (safety 0)))
  (let ((editable (hmcn-ensure-editable node edit)))
    (setf (aref (hmcn-array editable) i) a)
    (when j (setf (aref (hmcn-array editable) j) b))
    editable))

(defun node-assoc-edit-collision-node (node edit shift hash key val added-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type uint hash)
	   (type hash-map-collision-node node))
  (with-accessors ((this-hash hmcn-hash)
		   (array hmcn-array)
		   (count hmcn-count))
      node
    (if (= hash this-hash)
	(let ((idx (hmcn-find-index node key)))
	  (declare (type fixnum idx))
	  (cond ((/= idx -1)
		 (if (equal val (aref array (1+ idx)))
		     node
		     (hmcn-edit-and-set node edit (1+ idx) val)))

		((> (cl:length array) (* 2 count))
		 (setf (box-val added-leaf) added-leaf)
		 (let* ((2count (* 2 count))
			(editable (hmcn-edit-and-set node edit
						     2count key
						     (the uint (1+ 2count)) val)))
		   (declare (type uint 2count))
		   (incf (hmcn-count editable))
		   editable))))

	(node-assoc-edit
	 (make-hash-map-bitmap-node :edit edit :bitmap (bitpos this-hash shift)
				    :array (make-array 4 :initial-contents (list nil node nil nil)))
	 edit shift hash key val added-leaf))))

(defmethod node-without-edit ((node hash-map-collision-node) edit shift hash key removed-leaf)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (let ((idx (hmcn-find-index node key))
	(count (hmcn-count node)))
    (declare (type uint count)
	     (type fixnum idx))
    (if (= idx -1)
	node
	(progn
	  (setf (box-val removed-leaf) removed-leaf)
	  (unless (= 1 count)
	    (let* ((editable (hmcn-ensure-editable node edit))
		   (array (hmcn-array editable))
		   (2count (* 2 count))
		   (2idx (* 2 idx)))
	      (declare (type uint 2count))
	      (setf (aref array idx) (aref array (- 2count 2)))
	      (setf (aref array (1+ idx)) (aref array (1- 2count)))
	      (setf (aref array (- 2idx 2)) nil)
	      (setf (aref array (1- 2idx)) nil)
	      (decf (hmcn-count editable))
	      editable))))))

(defmethod node-find ((node hash-map-collision-node) shift hash key not-found)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum shift))
  (let ((idx (hmcn-find-index node key)))
    (declare (type fixnum idx))
    (if (< idx 0)
	not-found
	(aref (hmcn-array node) (1+ idx)))))

(defmethod node-make-iterator ((node hash-map-collision-node))
  (node-make-array-iterator (hmcn-array node)))

;; Print

(defmethod print-object ((map persistent-hashtrie) stream)
  (declare (optimize (speed 3) (safety 0))
	   (type stream stream))
  (with-accessors ((count phm-count))
      map
    (write-char #\{ stream)
    (loop with print-length = (and (not *print-readably*) *print-length*)
	  with itr of-type (function ()) = (map-make-iterator map)
	  for (remaining key val) = (multiple-value-list (funcall itr))
	    then (list nremaining nkey nval)
	  for (nremaining nkey nval) = (if remaining
					   (multiple-value-list (funcall itr))
					   (list nil nil nil))
	    then (multiple-value-list (funcall itr))
	  
	  for cnt from 0
	  while remaining
	  when (and (numberp print-length) (>= cnt print-length))
	    do
	       (princ "..." stream)
	       (return)
	  end
	  do (prin1 key stream)
	     (write-char #\  stream)
	     (prin1 val stream)
	     (when nremaining (princ ", " stream)))
    (write-char #\} stream)))

(defun map-assoc (map key value)
  (declare (optimize (speed 3) (safety 0))
	   (type hashtrie map))
  (typecase map
    (persistent-hashtrie (persistent-map-assoc map key value))
    (transient-hashtrie (transient-map-assoc map key value))))

(defun make-hashtrie (&rest args)
  (declare (optimize (speed 3) (safety 0)))
  (let ((r (phm-as-transient *empty-hash-map*)))
    (loop for (key val) on args by #'cddr
	  do (setf r (map-assoc r key val)))
    (thm-persistent r)))

(defun map-map (map fn)
  (declare (optimize (speed 3) (safety 0))
	   (type hashtrie map)
	   (type function fn))
  (loop with itr of-type (function ()) = (map-make-iterator map)
	for (remaining key val) = (multiple-value-list (funcall itr))
	while remaining collect (funcall fn key val)))

(defun map-reduce (map fn &optional start-val)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn)
	   (type hashtrie map))
  (loop with itr of-type (function ()) = (map-make-iterator map)
	for (remaining key val) = (multiple-value-list (funcall itr))
	while remaining
	for result = (funcall fn start-val key val)
	  then (funcall fn result key val)
	finally (return result)))
