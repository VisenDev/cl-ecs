;(setq *block-compile-default* t)
;(load "helpers.lisp")
(defpackage #:ecs
  (:use :cl)
  (:export #:ecs #:new-entity #:set-component! #:get-component)
  (:local-nicknames (#:tu #:type-utils) (#:ss #:sparse-set)))

(in-package #:ecs)

(defparameter *default-capacity* 32)
(defstruct ecs
  (available-ids
   (loop for id from 0 below *default-capacity* collect id)
   :type list)
  (num-allocated-ids *default-capacity* :type integer)
  (database (make-hash-table) :type hash-table))

(defun new-entity! (ecs)
  ;(assert (not (null ecs)))
	(let ((len (ecs-num-allocated-ids ecs)))
  (when (<= (length (ecs-available-ids ecs)) 0)
    (symbol-macrolet
	((len (ecs-num-allocated-ids ecs)))
    (push (ecs-avaiable-ids ecs) (append (ecs-available-ids ecs)
	    (loop for id from len below (* len 2) collect id)))))
  (the integer (pop (ecs-available-ids ecs))))


(defun get-components (ecs component-type)
  `(gethash ,component-type (ecs-database ,ecs)))

;(declaim (ftype (function (ecs integer keyword t)) set-component))
(defun set-component! (ecs entity component-type component-value)
  (let ((ss (get-components ecs component-type)))
      (when (null ss)
	(setf ss (ss:make-sparse-set component-type)))
      (ss:set! ss entity component-value)))


(defun get-component (ecs entity component-type)
  (ss:get (get-components ecs component-type) entity))
