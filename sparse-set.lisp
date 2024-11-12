(load "helpers.lisp")

(defpackage :sparse-set
  (:use :common-lisp :helpers)
  (:export
   :ss-test
   :ss-set!
   :ss-unset!
   :ss-get))

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

(defmacro ss-get-sparse (set sparse-index)
  "Returns a reference to the value in the sparse array"
  `(aref (ss-sparse ,set) ,sparse-index))

(defmacro ss-get-dense (set sparse-index)
  "Returns a reference to the value in the dense array"
  `(aref (ss-dense ,set) (ss-get-sparse ,set ,sparse-index)))

(defun ss-dense-initialize! (set)
  "initialize the dense array"
  (assert (not (null (ss-element-type set))))
  (setf (ss-dense set) (make-dynamic-array (ss-element-type set))))

(defmacro ss-ensure-dense-initialized! (set)
  "Checks if dense has been initialized and initialize it if necessary"
  `(when (null (ss-dense ,set))
     (ss-dense-initialize! ,set)))

(defmacro ss-len (set)
  "Returns the length of the dense array"
  `(fill-pointer (ss-dense ,set)))

(defmacro ss-top-index (set)
  "Returns the index of the top of the dense array"
  `(1- (ss-len ,set)))

(defmacro ss-top (set)
  "Returns a reference to the top element of the dense array"
  `(aref (ss-dense ,set) (ss-top-index ,set)))

(defmacro ss-sparse-from-dense (set dense-index)
  "returns the sparse index pointing to a given dense index"
    `(gethash ,dense-index (ss-dense-to-sparse ,set)))

(defun ss-set-sparse! (set sparse-index value)
  "Sets a value in the sparse array"
  (setf (ss-get-sparse set sparse-index) value))

(defun ss-set-dense! (set sparse-index value)
  "Sets a value in the dense array"
  (setf (ss-get-dense set sparse-index) value))

(defun ss-append-dense! (set value)
  "Appends a new value to the dense array and returns its index"
(vector-push-extend value (ss-dense set)))


(defun ss-set! (set sparse-index value)
  "Add a new value to the set at index"
  (assert (not (null (ss-element-type set))))
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

(defun ss-unset! (set unset-sparse-index)
  (let (
	(unset-dense-index (ss-get-sparse set unset-sparse-index))
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
    (format t "~a~%" set)
    (ss-set! set 3 1)
    (ss-set! set 7 4)
    (ss-set! set 3 1)
    (ss-set! set 2 5)
    (format t "~a~%" set)
    (ss-unset! set 3)
    (format t "~a~%" set)))


;export
;(let ((pack (find-package :sparse-set)))
;  (do-all-symbols
;      (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
