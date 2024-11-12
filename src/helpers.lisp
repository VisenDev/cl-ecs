(ql:quickload :closer-mop)

(defpackage :helpers
  (:use :cl)
  (:export
   :export-defun
   :get-struct-field-names
   :unwrap-fields
   :make-dynamic-array
   :assert-not-null))

(in-package :helpers)

(defmacro export-defun (name args &body body)
  "Defines a function and exports it from the current package."
  `(progn
     (defun ,name ,args ,@body)
     (export ',name)))


(defun get-struct-field-names (struct-type)
  "Return a list of all of the field names in a struct"
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots struct-type)))

(defmacro unwrap-fields ((struct-instance struct-type) &body body)
  "Create shorthand accessors for all of a struct's fields."
  `(symbol-macrolet
       (,@(mapcar
           (lambda (field-name)
             `(,field-name
               (,(find-symbol (concatenate 'string
                                      (symbol-name struct-type)
                                      "-"
                                      (symbol-name field-name)))
                ,struct-instance)))
           (get-struct-field-names struct-type)))
     ,@body))

(defun make-dynamic-array (element-type) 
  (make-array
    0 
    :element-type element-type
    :adjustable t
    :fill-pointer 0))


(defun assert-not-null (&rest values)
  (dolist (value values)
    (assert (not (null value)))))

;(defun export-all-symbols (package-name)
;  (let ((pack (find-package package-name)))
;    (do-all-symbols
;	(sym pack)
;      (when (eql (symbol-package sym) pack) (export sym)))))


;(export-all-symbols :helpers)
