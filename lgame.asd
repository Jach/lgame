(defsystem #:lgame
  :description "PyGame-inspired library to help make Lisp games with SDL2"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "Public Domain"
  :depends-on (#:cffi #:cl-autowrap #:sdl2 #:sdl2-image #:sdl2-mixer #:sdl2-ttf
               #:alexandria #:livesupport
               #:cl-annot
               #:pileup
               )
  :components ((:module "src/"
                        :serial t
                        :components ((:file "packages")
                                     (:file "state")
                                     (:file "globals")
                                     (:file "display")
                                     (:file "event")
                                     (:file "mouse")
                                     (:file "time")

                                     (:file "rect")
                                     (:file "sprite")
                                     (:file "font")
                                     (:file "texture-loader")

                                     (:file "pathfinding")

                                     (:file "init")
                                     ))))
