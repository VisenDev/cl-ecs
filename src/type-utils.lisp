;(ql:quickload :closer-mop)

(defpackage :type-utils
  (:use :cl)
  (:export
   :export-defun
   :get-struct-field-names
   :unwrap-fields
   :make-dynamic-array
   :assert-not-null
   :declaim-type
   :declare-type
   :typed-defun))

(in-package :type-utils)

(defmacro export-defun (name args &body body)
  "Defines a function and exports it from the current package."
  `(progn
     (defun ,name ,args ,@body)
     (export ',name)))


;(defun get-struct-field-names (struct-type)
;  "Return a list of all of the field names in a struct"
;  (mapcar #'closer-mop:slot-definition-name
;          (closer-mop:class-slots struct-type)))
;
;(defmacro unwrap-fields ((struct-instance struct-type) &body body)
;  "Create shorthand accessors for all of a struct's fields."
;  `(symbol-macrolet
;       (,@(mapcar
;           (lambda (field-name)
;             `(,field-name
;               (,(find-symbol (concatenate 'string
;                                      (symbol-name struct-type)
;                                      "-"
;                                      (symbol-name field-name)))
;                ,struct-instance)))
;           (get-struct-field-names struct-type)))
;     ,@body))

(defun make-dynamic-array (element-type) 
  (make-array
    0 
    :element-type element-type
    :adjustable t
    :fill-pointer 0))


(defun assert-not-null (&rest values)
  (dolist (value values)
    (assert (not (null value)))))

(defmacro declaim-type (name type)
  `(declaim (type (,type) ,name)))

(defmacro declare-type (name type)
  `(declare (type (,type) ,name)))

(defmacro declaim-signature (function-name parameter-types return-type)
  `(declaim (ftype (function ,parameter-types ,return-type) ,function-name)))

(defmacro typed-defun (return-type function-name parameters &body body)
  "defines a function and declaims its type signature"
  (assert (evenp (length parameters)))
  (format t "expanding macro...~%")
  (let
      ((parameter-types '())
       (parameter-names '())
       (parameter-state :expect-type))
    (dolist (type-or-name parameters)
      (case parameter-state
	(:expect-name
	 (assert (typep type-or-name 'symbol))
	 (push type-or-name parameter-names)
	 (setf parameter-state :expect-type))
	(:expect-type
	 (assert (typep type-or-name 'symbol))
	 (let ((type (find-symbol (symbol-name type-or-name))))
	   (push type parameter-types))
	 (setf parameter-state :expect-name))))
    `(progn
       (declaim (ftype (function ,(nreverse parameter-types) ,(find-symbol (symbol-name return-type))) ,function-name))
       (defun ,function-name (,@(nreverse parameter-names)) ,@body))))
	       


;(typed-defun :t mynewfun (:string value :integer value2)
;  (format t "~a~%" value)
;  (format t "~a~%" value2)
;  nil)

;(mynewfun "hi" (the integer 1))

;(defmacro constlet (form &body body)
;  (let
;      ((form-name (first form))
;       (form-value (second form))
;       (gensym-name (gensym)))
;    `(progn
;       (defconstant ,gensym-name ,form-value)
;       (symbol-macrolet
;           ((,form-name ,gensym-name))
;         ,@body))))
;
;(constlet (mylist (list 1 2 3 4 5))
;  (write mylist)
;  ;(push 4 mylist) ;causes compile error
;  (write mylist))










;(defun export-all-symbols (package-name)
;  (let ((pack (find-package package-name)))
;    (do-all-symbols
;	(sym pack)
;      (when (eql (symbol-package sym) pack) (export sym)))))


;(export-all-symbols :helpers)

(ql:quickload 'alexandria)

(defparameter *myvar* (let ((myvar 100))
  (lambda () myvar)))

(defmacro constlet (name value &body body)
  (let
      ((name-gensym (gensym))
       (function-gensym (gensym)))
    `(let ((,name-gensym ,value))
       (flet
	   ((,function-gensym () ,name-gensym))
	 (symbol-macrolet ((,name (,function-gensym)))
	   ,@body)))))


(defmacro constlet (forms &body body)
  (let 
      ;;gensym a name for each form
      ((gensyms
	 (mapcar
	  #'(lambda (form)
	      (declare (ignore form))
	      (gensym))
	  forms))

       ;;create an accessor function name for each value
       (function-names
	 (mapcar
	  #'(lambda (form)
	      (intern
	       (concatenate
		'string
		(symbol-name (first form))
		"-IS-READ-ONLY"
		)))
	  forms))
       )
    ;;assign each value to a gensym
    `(let
	 ,(mapcar
	   #'(lambda (my-gensym form)
	       `(,my-gensym ,(second form)))
	   gensyms forms)
       ;;define each accessor function so it can access the value assigned to its corresponding gensym
       (flet
	   ,(mapcar
	     #'(lambda (my-gensym function-name)
		 `(,function-name () ,my-gensym))
	     gensyms function-names)

	 ;;create a symbol macro that calls the accessor function
	 (symbol-macrolet
	     ,(mapcar
	       #'(lambda (function-name form)
		   `(,(first form) (,function-name)))
	       function-names forms)
	   ,@body)))))


(defstruct vec2 x y)

(constlet
    ((bip (list 1 2 3 4 5)))
  (write bip)
  (write (nreverse bip)))
