(defstruct sparse-set
  indexes
  items)

(defun init-sparse-set (&optional (maximum-capacity 1024))
  (make-sparse-set
   :items (make-array maximum-capacity :fill-pointer 0)
   :indexes (make-array maximum-capacity :fill-pointer maximum-capacity :initial-element nil)))

(defun set-sparse-set (set sparse-index value)
  (declare (type integer sparse-index)
	   (type sparse-set set))
  (symbol-macrolet
      ((indexes (sparse-set-indexes set))
       (items (sparse-set-items set))
       (sparse-index-reference (aref indexes sparse-index))
       (next-dense-index (fill-pointer items))
       (dense-index-reference (aref items sparse-index-reference))
       )
    (if (null sparse-index-reference)
	(progn 
	  (setf sparse-index-reference next-dense-index)
	  (incf (fill-pointer (sparse-set-items set))))
	)
    (setf dense-index-reference value)
    ))

(defun make-ecs () (list))

(defun set-component (ecs entity component-type component-value)
  (declare (type list ecs) (type integer entity) (type symbol component-type))
  (symbol-macrolet
      ((ecs-field (getf ecs component-type)))
    (if (null ecs-field)
	(setq ecs (append ecs (list component-type (init-sparse-set)))))
    (set-sparse-set ecs-field entity component-value))
  ecs
  )
  
