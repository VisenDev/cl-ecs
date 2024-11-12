(load "helpers.lisp")
(load "sparse-set.lisp")

;;;==============ENTITY================
(defstruct entity-archive
  (id nil :type integer)
  (components nil :type list))
  

;;;==============COMPONENTS==============
(defmacro make-component-array (type)
  `(make-dynamic-array ,type))
  
(deftype component-id () 'string)
(defmacro make-component-id-array ()
  `(make-dynamic-array 'component-id))

(defstruct component-archive
  (id nil :type string)
  value)

(defun get-component-archive-names (component-archive-list)
  (mapcar #'component-archive-id component-archive-list))


;;;================ARCHETYPES================
(defstruct archetype
  "an archetype refers to a specific set of shared components"
  component-ids ;list of ids of components
  id ;string name of archetype
  (database (make-hash-table));database of component values for stored entities
  (entity-id-to-index (make-sparse-set :element-type 'integer))) 


(defun get-archetype-id (archetype-component-ids)
  "Get the id of an archetype given a list of its components"
  (reduce
   (lambda (a b) (concatenate 'string a "-" b))
   (sort (copy-seq archetype-component-ids) #'string-lessp)))

(defun move-entity-to-archetype (archetype entity-id entity-components)
    (assert (equalp
	     (get-archetype-id (archetype-component-ids archetype))
	     (get-archetype-id (get-component-archive-names entity-components))))
  (let ((database (archetype-database archetype)))
    (dolist (archived entity-components)
      (symbol-macrolet ((component-storage
			  (gethash (component-archive-id archived) database)))
	(when (nullp component-storage)
	  (setf component-storage (list))))
    ;todo: finish code
      )))


;;;================ECS=====================
(defparameter *default-ecs-capacity* 1024)
(defstruct ecs
  (archetypes (let ((archetypes (make-hash-table)))
		(setf (gethash "" archetypes) (make-archetype)) archetypes))
  (available-ids (loop for id from 0 below *default-ecs-capacity* collect id)))

(defun new-entity! (ecs)
  (let ((id (pop (ecs-available-ids ecs))))))
    