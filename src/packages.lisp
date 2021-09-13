(in-package #:cl-user)

(defpackage #:lgame
  (:shadowing-import-from #:sdl2-ffi.functions #:sdl-thread-id)
  (:use #:common-lisp #:sdl2-ffi.functions #:sdl2-ffi #:annot.std #:annot.class)
  (:documentation
    "Lgame is composed of several packages of the form lgame.foo, where foo generally corresponds
     to a filename. The top-level lgame package itself is for re-exporting lgame.state,
     and to :use but not export all symbols in sdl2-ffi.functions and sdl2-ffi."))

(defpackage #:lgame.sprite
  (:use #:common-lisp #:annot.std #:annot.class)
  (:documentation
    "WIP sprite module, inspired by pygame.sprite. The idea is to provide a base Sprite
     class that sprite objects can inherit from and optionally override their own update
     and draw methods to use the image/rect slots differently. Additionally a Group class
     concept is provided to manage collections of sprites."))
