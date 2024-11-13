;(load "helpers.lisp")

(defpackage :sparse-set
  (:use :common-lisp :helpers)
  (:export
   :ss-test
   :ss-set!
   :ss-unset!
   :ss-get
   :ss-not-nil-sparse
   :make-sparse-set))

(in-package :sparse-set)

(defconstant +ss-default-capacity+ 8)
(defstruct (sparse-set
  (:conc-name ss-))
  (dense-to-sparse (make-hash-table))
  element-type
  (dense nil)
  (sparse
   (make-array +ss-default-capacity+ 
    :fill-pointer +ss-default-capacity+ 
    :initial-element nil
    :adjustable t)))

(typed-defun :vector ss-not-nil-sparse (:sparse-set set)
  "Returns a list of all non-nil values in the sparse array"
  (remove-if #'null (ss-sparse set)))

(typed-defun :t ss-audit (:sparse-set set)
  "audit the sparse set to ensure state is valid"
  
  (declare (optimize (safety 3)))

  ;;Assert sparse-set fields are defined
  (assert-not-null (ss-sparse set) (ss-dense set) (ss-element-type set) (ss-dense-to-sparse set))

  ;Assert there are the same number of dense indexes as non-nil sparse indexes
  (let
      ((num-sparse-entries (length (ss-non-nil-sparse set)))
       (num-dense-entries (fill-pointer (ss-dense set))))
    (assert (equalp num-sparse-entries num-dense-entries)))

  ;;;Assert all sparse indexes point to a valid and unique dense index
  (let
      ((unique-items (list)))
  (loop for item across (the vector (ss-non-nil-sparse set)) do
    (assert (not (member item unique-items))) ;ensure uniqueness
    (assert (>= item 0 )) ;assert gte than 0
    (assert (< item (fill-pointer (ss-dense set)))) ;assert less than length of dense array

    (push item unique-items))))

(defmacro ss-get-sparse (set sparse-index)
  "Returns a reference to the value in the sparse array"
  `(aref (ss-sparse ,set) ,sparse-index))

(defmacro ss-get-dense (set sparse-index)
  "Returns a reference to the value in the dense array"
  `(aref (ss-dense ,set) (ss-get-sparse ,set ,sparse-index)))

(typed-defun :t ss-dense-initialize! (:sparse-set set)
  (assert-not-null (ss-element-type set))
  (setf (ss-dense set) (make-dynamic-array (ss-element-type set))))

(typed-defun :t ss-ensure-dense-initialized! (:sparse-set set)
  "Checks if dense has been initialized and initialize it if necessary"
  (when (null (ss-dense set))
     (ss-dense-initialize! set)))

(typed-defun :integer ss-len (:sparse-set set)
  "Returns the length of the dense array"
  (fill-pointer (ss-dense set)))

(typed-defun :integer ss-top-index (:sparse-set set)
  "Returns the index of the top of the dense array"
  (1- (ss-len set)))

(defmacro ss-top (set)
  "Returns a reference to the top element of the dense array"
  `(aref (ss-dense ,set) (ss-top-index ,set)))

(defmacro ss-sparse-from-dense (set dense-index)
  "returns the sparse index pointing to a given dense index"
    `(gethash ,dense-index (ss-dense-to-sparse ,set)))

(typed-defun :t ss-set-sparse!
    (:sparse-set set :integer sparse-index :t value)
  "Sets a value in the sparse array"
  (setf (ss-get-sparse set sparse-index) value))

(typed-defun :t ss-set-dense!
    (:sparse-set set :integer sparse-index :t value)
  "Sets a value in the dense array"
  (setf (ss-get-dense set sparse-index) value))

(typed-defun :t ss-append-dense! (:sparse-set set :t value)
  "Appends a new value to the dense array and returns its index"
  (assert-not-null value)
(vector-push-extend value (ss-dense set)))


(typed-defun :t ss-set! (:sparse-set set :integer sparse-index :t value)
  "Add a new value to the set at index"
  (assert-not-null sparse-index value
		  (ss-element-type set))
  (assert (typep value (ss-element-type set)))
  (ss-ensure-dense-initialized! set)
  (if (null (ss-get-sparse set sparse-index))
      (let ((dense-index (ss-append-dense! set value)))
	(ss-set-sparse! set sparse-index dense-index) ;set sparse
	(setf ;record the dense to sparse relationship
	 (gethash dense-index (ss-dense-to-sparse set))
	 sparse-index)
	)
      (ss-set-dense! set sparse-index value)))

(typed-defun :t ss-unset! (:sparse-set set :integer unset-sparse-index)
  "Unset the value at index"
  (let
      ((unset-dense-index (ss-get-sparse set unset-sparse-index))
	(top-sparse-index (ss-sparse-from-dense set (ss-top-index set))))
    (setf
     (ss-get-dense set unset-sparse-index)
     (ss-top set)) ;copy dense-top into unset-dense-index location
    (ss-set-sparse!
     set top-sparse-index unset-dense-index) ;update sparse entry for top
    (if (equalp unset-dense-index (ss-top-index set))
	(setf (gethash unset-dense-index (ss-dense-to-sparse set)) nil)
	(setf ;update the dense to sparse relationship for top
	 (gethash unset-dense-index (ss-dense-to-sparse set))
	 top-sparse-index))
    (decf (fill-pointer (ss-dense set))) ;decrement fill pointer
    (ss-set-sparse! set unset-sparse-index nil)))




(defun ss-test ()
  (let ((set (make-sparse-set :element-type 'integer)))

    (format t "Initial set: ~a~%" set)

    (format t "Setting 3 to 1...~%")
    (ss-set! set 3 1)
    (format t "set: ~a~%~%" set)

    (ss-audit set)

    (format t "Setting 7 to 2...~%")
    (ss-set! set 7 2)
    (format t "set: ~a~%~%" set)

    (ss-audit set)

    (format t "Setting 5 to 3...~%")
    (ss-set! set 5 3)
    (format t "set: ~a~%~%" set)


    (ss-audit set)


    (format t "Setting 2 to 5...~%")
    (ss-set! set 2 5)
    (format t "set: ~a~%~%" set)


    (ss-audit set)

    (format t "Unsetting 3...~%")
    (ss-unset! set 3)
    (format t "~a~%" set)


    (ss-audit set)

    (format t "Unsetting 2...~%")
    (ss-unset! set 2)
    (format t "~a~%" set)


    (ss-audit set)

    (format t "Setting 3 to 1...~%")
    (ss-set! set 3 1)
    (format t "set: ~a~%~%" set)

    (ss-audit set)

    ))


;export
;(let ((pack (find-package :sparse-set)))
;  (do-all-symbols
;      (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
