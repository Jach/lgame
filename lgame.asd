(defsystem "lgame"
  :description "Pygame-inspired library to help make Lisp games with SDL2"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "Public Domain / Unlicense"
  :depends-on ("cffi" "cl-autowrap/libffi" "sdl2" "sdl2-image" "sdl2-mixer"
               "alexandria" "livesupport"
               "cl-annot"

               "font-discovery"

               "pileup" ; for A*
               )
  :components ((:module "sdl2-ttf/"
                        :serial t
                        :components ((:file "wrap")
                                     (:static-file "SDL_ttf.h")))

               (:module "sdl2-ttf-spec"
                        :pathname "sdl2-ttf/spec")

               (:module "src/"
                        :serial t
                        :depends-on ("sdl2-ttf/")
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
                                     (:file "render")
                                     (:file "draw")

                                     (:file "texture-loader")

                                     (:file "pathfinding")

                                     (:file "util")

                                     (:file "init")
                                     ))))
