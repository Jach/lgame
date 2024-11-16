(defsystem "lgame"
  :description "Pygame-inspired library to help make Lisp games with SDL2"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "Public Domain / Unlicense"
  :depends-on ("cffi" "cl-autowrap/libffi" "sdl2" "sdl2-image" "sdl2-mixer" ; core sdl deps and ffi wrapping...
               "alexandria" "livesupport" ; development enhancements
 ;              "trivial-clock" ; more portable nanosecond clock precision? will also be part of sdl3...
               "font-discovery" ; easier to use system fonts
               "pileup" ; for A*, will probably remove eventually
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
