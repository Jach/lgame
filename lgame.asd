(defsystem "lgame"
  :description "Pygame-inspired library to help make Lisp games with SDL2"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "Public Domain / Unlicense"
  :depends-on ("cffi" "cl-autowrap" "sdl2" "sdl2-image" "sdl2-mixer" ; core sdl deps and ffi wrapping...
               "alexandria" "livesupport" ; development enhancements
 ;              "trivial-clock" ; more portable nanosecond clock precision? will also be part of sdl3...
               "font-discovery" ; easier to use system fonts
               "pileup" ; for A*, will probably remove eventually
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
                                     (:file "box")
                                     (:file "sprite")
                                     (:file "font.ffi")
                                     (:file "font")
                                     (:file "render")
                                     (:file "draw")

                                     (:file "texture-loader")

                                     (:file "pathfinding")

                                     (:file "util")

                                     (:file "init")
                                     )))
  :in-order-to ((asdf:test-op (asdf:test-op "lgame/test"))))

(defsystem "lgame/test"
  :depends-on ("lgame"
               "str"
               "fiveam")
  :serial t
  :pathname "test"
  :components ((:file "box-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call ':5am '#:run-all-tests ':summary ':suite)))
