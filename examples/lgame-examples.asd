(defsystem "lgame-examples"
  :depends-on ("lgame" "cl-opengl")
  :serial t
  :components ((:file "aliens/aliens")
               (:file "chimp/chimp")
               (:file "gl/gl")
               (:file "gl2/gl2")
               (:file "liquid/liquid")
               (:file "maze/maze")
               (:file "moveit/moveit")
               (:file "testsprite/testsprite")
               (:file "vgrade/vgrade")
               ))

