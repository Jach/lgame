(in-package #:cl-user)

(defpackage #:lgame.state
  (:use #:common-lisp)
  (:documentation
    "This package is to manage state shared among the various lgame modules and a game.
     lgame is currently designed to only handle one game at a time, so there is typically
     just one window/screen, one renderer, one resource manager for each resource type...")
  (:export #:*screen*
           #:*screen-rect*
           #:*renderer*
           #:*texture-loader*))

(defpackage #:lgame
  (:shadowing-import-from #:sdl2-ffi.functions #:sdl-thread-id)
  (:use #:common-lisp #:sdl2-ffi.functions #:sdl2-ffi #:annot.std #:annot.class
        #:lgame.state)
  (:documentation
    "lgame is composed of several packages of the form lgame.foo, where foo generally corresponds
     to a filename. The top-level lgame package itself is for re-exporting lgame.state,
     and to :use but not export all symbols in sdl2-ffi.functions and sdl2-ffi.")
  (:export #:init
           #:quit
           #:*screen*
           #:*screen-rect*
           #:*renderer*
           #:*texture-loader*))

(defpackage #:lgame.display
  (:use #:common-lisp)
  (:documentation
    "Provides functions to set up an initial display for the game.")
  (:export #:create-window
           #:create-centered-window
           #:create-renderer
           #:set-logical-size
           #:screenshot-png))

(defpackage #:lgame.event
  (:use #:common-lisp #:annot.std)
  (:documentation
    "Provides utils and wrappers around SDL2 events, particularly handling the event loop nicely with 'do-event
     and being able to reference event data with 'ref."))

(defpackage #:lgame.mouse
  (:use #:common-lisp #:annot.std)
  (:documentation
    "Provides a function to get the current mouse position."))

(defpackage #:lgame.time
  (:use #:common-lisp)
  (:documentation
    "Provides functions to handle time-related tasks like fixing a max FPS framerate."))

(defpackage #:lgame.rect
  (:use #:common-lisp #:annot.std)
  (:documentation
    "Utils around handling SDL_Rects. Several with- macros like 'with-rect are included to efficiently
     use a stack-allocated object instead of allocating and freeing, though for any longer-lasting rects
     one should use the usual 'sdl2:make-rect and 'free-rect. Also included are some intuitive lookup/setter functions,
     and collision detection routines."))

(defpackage #:lgame.sprite
  (:use #:common-lisp #:annot.std #:annot.class)
  (:documentation
    "WIP sprite module, inspired by pygame.sprite. The idea is to provide a base Sprite
     class that sprite objects can inherit from and optionally override their own update
     and draw methods to use the image/rect slots differently. Additionally a Group class
     concept is provided to manage collections of sprites."))

(defpackage #:lgame.font
  (:use #:common-lisp #:annot.std)
  (:documentation
    "Wrapper around sdl2-ttf, loads (and caches) fonts, provides a default font, and a render-text function that returns
     a texture."))

(defpackage #:lgame.loader
  (:use #:common-lisp #:annot.std #:annot.class)
  (:documentation
    "Provides loader wrappers for various assets (currently just *texture-loader*) that may be easier
     than calling the underlying functions directly."))

(defpackage #:lgame.pathfinding
  (:use #:common-lisp #:annot.std #:annot.class)
  (:documentation
    "Provides a fairly generic A* algorithm protocol for grids with several customization knobs."))

(defpackage #:lgame.util
  (:use #:common-lisp #:annot.std)
  (:documentation
    "Utility functions that don't fit in other packages, like printing out license information."))
