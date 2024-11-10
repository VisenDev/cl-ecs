(defstruct sparse-set
  indexes
  items)

(defun init-sparse-set (&optional (maximum-capacity 1024))
  (make-sparse-set
   :items (make-array 8 :fill-pointer 0 :adjustable t)
   :indexes (make-array maximum-capacity
			:fill-pointer maximum-capacity
			:initial-element nil
			:adjustable t)))

(declaim (ftype (function (sparse-set integer t) sparse-set) set-sparse-set))
(defun set-sparse-set (set sparse-index value)
  (when (< sparse-index 0) (error "index must be above 0"))
  (symbol-macrolet
      ((indexes (sparse-set-indexes set))
       (items (sparse-set-items set)))
    (if (null (aref indexes sparse-index))
      (progn 
	(setf (aref indexes sparse-index) (fill-pointer items))
	(vector-push value items))
      (progn
	(setf (aref items (aref indexes sparse-index)) value))))
  set)

(deftype ecs () '(satisfies listp))
(defun make-ecs () (list))

(declaim (ftype (function (ecs integer keyword t)) set-component))
(defun set-component (ecs entity component-type component-value)
  (let ((result ecs))
    (symbol-macrolet
	((my-sparse-set (getf result component-type)))
      (when (null my-sparse-set)
	(setf result (append ecs (list component-type (init-sparse-set)))))
      (assert (not (null my-sparse-set)))
      (set-sparse-set my-sparse-set entity component-value))
    result)
  )
  
