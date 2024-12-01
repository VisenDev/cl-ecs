;(load "helpers.lisp")

(defpackage :sparse-set
  (:use #:common-lisp)
  (:local-nicknames
   (#:tu #:type-utils))
  (:export
   #:test
   #:set!
   #:unset!
   #:get
   #:not-nil-sparse
   #:sparse-set
   #:make-sparse-set))

(in-package :sparse-set)

(declaim (inline make-sparse-set make-sparse-set-base))
(defstruct (sparse-set
	    (:conc-name ss-)
	    (:constructor make-sparse-set-base) )
  (dense-to-sparse nil :type hash-table)
  (element-type nil :read-only t :type symbol)
  (dense nil :type vector)
  (sparse nil :type vector))

(defun make-sparse-set (element-type)
  "Constructor for sparse-set"
  (let ((default-capacity 16))
    (make-sparse-set-base
     :dense-to-sparse (make-hash-table)
     :element-type element-type
     :dense (tu:make-dynamic-array element-type)
     :sparse
     (make-array
      default-capacity
      :fill-pointer 0
      :initial-element nil
      :adjustable t))))

(tu:typed-defun :vector not-nil-sparse (:sparse-set set)
  "Returns a list of all non-nil values in the sparse array"
  (remove-if #'null (ss-sparse set)))

(tu:typed-defun :t audit (:sparse-set set)
  "audit the sparse set to ensure state is valid"
  
  (declare (optimize (safety 3)))

  ;;Assert sparse-set fields are defined
  (tu:assert-not-null (ss-sparse set) (ss-dense set) (ss-element-type set) (ss-dense-to-sparse set))

  ;Assert there are the same number of dense indexes as non-nil sparse indexes
  (let
      ((num-sparse-entries (length (not-nil-sparse set)))
       (num-dense-entries (fill-pointer (ss-dense set))))
    (assert (equalp num-sparse-entries num-dense-entries)))

  ;;;Assert all sparse indexes point to a valid and unique dense index
  (let
      ((unique-items (list)))
  (loop for item across (the vector (not-nil-sparse set)) do
    (assert (not (member item unique-items))) ;ensure uniqueness
    (assert (>= item 0 )) ;assert gte than 0
    (assert (< item (fill-pointer (ss-dense set)))) ;assert less than length of dense array

    (push item unique-items))))

(defmacro get-sparse (set sparse-index)
  "Returns a reference to the value in the sparse array"
  `(aref (ss-sparse ,set) ,sparse-index))

(defmacro get-dense (set sparse-index)
  "Returns a reference to the value in the dense array"
  `(aref (ss-dense ,set) (get-sparse ,set ,sparse-index)))

(tu:typed-defun :t dense-initialize! (:sparse-set set)
  (tu:assert-not-null (ss-element-type set))
  (setf (ss-dense set) (tu:make-dynamic-array (ss-element-type set))))

(tu:typed-defun :integer len (:sparse-set set)
  "Returns the length of the dense array"
  (fill-pointer (ss-dense set)))

(tu:typed-defun :integer top-index (:sparse-set set)
  "Returns the index of the top of the dense array"
  (1- (len set)))

(defmacro top (set)
  "Returns a reference to the top element of the dense array"
  `(aref (ss-dense ,set) (top-index ,set)))

(defmacro sparse-from-dense (set dense-index)
  "returns the sparse index pointing to a given dense index"
    `(gethash ,dense-index (ss-dense-to-sparse ,set)))

(tu:typed-defun :t set-sparse!
    (:sparse-set set :integer sparse-index :t value)
  "Sets a value in the sparse array"
  (setf (get-sparse set sparse-index) value))

(tu:typed-defun :t set-dense!
    (:sparse-set set :integer sparse-index :t value)
  "Sets a value in the dense array"
  (setf (get-dense set sparse-index) value))

(tu:typed-defun :t append-dense! (:sparse-set set :t value)
  "Appends a new value to the dense array and returns its index"
  (tu:assert-not-null value)
(vector-push-extend value (ss-dense set)))


(tu:typed-defun :t set! (:sparse-set set :integer sparse-index :t value)
  "Add a new value to the set at index"
  (tu:assert-not-null sparse-index value
		  (ss-element-type set))
  (assert (typep value (ss-element-type set)))
  (if (null (get-sparse set sparse-index))
      (let ((dense-index (append-dense! set value)))
	(set-sparse! set sparse-index dense-index) ;set sparse
	(setf ;record the dense to sparse relationship
	 (gethash dense-index (ss-dense-to-sparse set))
	 sparse-index)
	)
      (set-dense! set sparse-index value)))

(tu:typed-defun :t unset! (:sparse-set set :integer unset-sparse-index)
  "Unset the value at index"
  (let
      ((unset-dense-index (get-sparse set unset-sparse-index))
	(top-sparse-index (sparse-from-dense set (top-index set))))
    (setf
     (get-dense set unset-sparse-index)
     (top set)) ;copy dense-top into unset-dense-index location
    (set-sparse!
     set top-sparse-index unset-dense-index) ;update sparse entry for top
    (if (equalp unset-dense-index (top-index set))
	(setf (gethash unset-dense-index (ss-dense-to-sparse set)) nil)
	(setf ;update the dense to sparse relationship for top
	 (gethash unset-dense-index (ss-dense-to-sparse set))
	 top-sparse-index))
    (decf (fill-pointer (ss-dense set))) ;decrement fill pointer
    (set-sparse! set unset-sparse-index nil)))




(defun test ()
  (let ((set (make-sparse-set 'integer)))

    (format t "Initial set: ~a~%" set)

    (format t "Setting 3 to 1...~%")
    (set! set 3 1)
    (format t "set: ~a~%~%" set)

    (audit set)

    (format t "Setting 7 to 2...~%")
    (set! set 7 2)
    (format t "set: ~a~%~%" set)

    (audit set)

    (format t "Setting 5 to 3...~%")
    (set! set 5 3)
    (format t "set: ~a~%~%" set)


    (audit set)


    (format t "Setting 2 to 5...~%")
    (set! set 2 5)
    (format t "set: ~a~%~%" set)


    (audit set)

    (format t "Unsetting 3...~%")
    (unset! set 3)
    (format t "~a~%" set)


    (audit set)

    (format t "Unsetting 2...~%")
    (unset! set 2)
    (format t "~a~%" set)


    (audit set)

    (format t "Setting 3 to 1...~%")
    (set! set 3 1)
    (format t "set: ~a~%~%" set)

    (audit set)

    ))


;export
;(let ((pack (find-package :sparse-set)))
;  (do-all-symbols
;      (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
