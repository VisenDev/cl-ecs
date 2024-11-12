;(setq *block-compile-default* t)
(load "helpers.lisp")


(defconstant +default-capacity+ 32)
(defstruct ecs
  (available-ids
   (loop for id from 0 below +default-capacity+ collect id)
   :type list)
  (num-allocated-ids +default-capacity+ :type integer)
  (database (make-hash-table) :type hash-table))

(defun new-entity! (ecs)
  (assert (not (null ecs)))
  (when (<= (length (ecs-available-ids ecs)) 0)
    (symbol-macrolet
	((len (ecs-num-allocated-ids ecs)))
    (setf (ecs-avaiable-ids ecs) (append (ecs-available-ids ecs)
	    (loop for id from len below (* len 2) collect id)))))
  (the integer (pop (ecs-available-ids ecs))))


(defmacro get-components (ecs component-type)
  `(gethash ,component-type (ecs-database ,ecs)))

;(declaim (ftype (function (ecs integer keyword t)) set-component))
(defun set-component! (ecs entity component-type component-value)
    (symbol-macrolet
	((my-sparse-set (get-components ecs component-type)))
      (when (null my-sparse-set)
	(setf my-sparse-set (init-sparse-set)))
      (assert (not (null my-sparse-set)))
      (set-sparse-set! my-sparse-set entity component-value)))


(defun get-component (ecs entity component-type)
  (get-sparse-set (get-components ecs component-type) entity))
