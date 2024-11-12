(asdf:defsystem "ecs"
  :depends-on ("closer-mop")
  :serial t
  :components
  ((:file "src/helpers")
   (:file "src/sparse-set")
   (:file "src/archetype")))
  
