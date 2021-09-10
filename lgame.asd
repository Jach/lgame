(defsystem #:lgame
  :description "PyGame-inspired library to help make Lisp games with SDL2"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "Public Domain"
  :depends-on (#:cffi #:cl-autowrap/libffi #:sdl2 #:sdl2-image #:sdl2-mixer #:sdl2-ttf
               #:alexandria #:livesupport
               #:cl-annot
               )
  :components ((:module "src/"
                        :serial t
                        :components ((:file "package")
                                     (:file "globals")
                                     (:file "init")
                                     (:file "display")
                                     (:file "event")
                                     (:file "rect")
                                     (:file "time")
                                     (:file "sprite")
                                     (:file "font")
                                     (:file "texture-loader")
                                     ))))
