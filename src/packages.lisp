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
  (:use #:common-lisp #:sdl2-ffi.functions #:sdl2-ffi
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
           #:*texture-loader*
           #:lgame-error
           #:null-ptr?))

(defpackage #:lgame.display
  (:use #:common-lisp)
  (:documentation
    "Provides functions to set up an initial display for the game.")
  (:export #:create-window
           #:create-centered-window
           #:create-renderer
           #:set-logical-size
           #:window-pixel-format
           #:screenshot-png))

(defpackage #:lgame.event
  (:use #:common-lisp)
  (:documentation
    "Provides utils and wrappers around SDL2 events, particularly handling the event loop nicely with 'do-event
     and being able to reference event data with 'ref.")
     (:export #:do-event
              #:with-event
              #:ref
              #:event-type
              #:key-scancode
              #:key-pressed?
              #:modifier-pressed?))

(defpackage #:lgame.mouse
  (:use #:common-lisp)
  (:documentation
    "Provides a function to get the current mouse position.")
  (:export #:get-mouse-pos
           #:set-hover-cursor
           #:set-normal-cursor
           #:cleanup-cursors))

(defpackage #:lgame.time
  (:use #:common-lisp)
  (:documentation
    "Provides functions to handle time-related tasks like fixing a max FPS framerate.")
  (:export #:clock-start
           #:clock-stop
           #:clock-running?
           #:clock-tick
           #:clock-time
           #:dt
           #:*last-any-delay*
           #:*last-frame-duration*
           #:*tick-us*
           #:*tick-ms*
           #:*ticked-frames*))

(defpackage #:lgame.rect
  (:use #:common-lisp)
  (:import-from #:sdl2
                #:rect-width
                #:rect-height)
  (:export #:rect-width
           #:rect-height
           #:with-rect
           #:with-rect-from-rect
           #:with-inflated-rect
           #:with-clipped-rect
           #:with-moved-rect
           #:with-point
           #:rect-string
           #:move-rect
           #:set-rect
           #:rect-fields
           #:midway
           #:rect-coord
           #:get-texture-rect
           #:clamp
           #:collide-rect?
           #:collide-point?
           #:outside-screen?
           #:contains?)
  (:documentation
    "Utils around handling SDL_Rects. Several with- macros like 'with-rect are included to efficiently
     use a stack-allocated object instead of allocating and freeing, though for any longer-lasting rects
     one should use the usual 'sdl2:make-rect and 'free-rect. Also included are some intuitive lookup/setter functions,
     and collision detection routines."))

(defpackage #:lgame.sprite
  (:use #:common-lisp)
  (:documentation
    "WIP sprite module, inspired by pygame.sprite. The idea is to provide a base Sprite
     class that sprite objects can inherit from and optionally override their own update
     and draw methods to use the image/rect slots differently. Additionally a Group class
     concept is provided to manage collections of sprites.")
     (:export #:update
              #:draw
              #:kill
              #:cleanup
              #:add-groups
              #:remove-groups
              #:add-sprites
              #:remove-sprites
              #:map-sprite
              #:do-sprite
              #:remove-all-sprites
              #:sprite-count
              #:empty?
              #:sprite-collide
              #:group-collide
              #:group-query-class

              #:sprite
              #:.image
              #:.rect
              #:.angle
              #:.flip
              #:.alive?
              #:.groups

              #:cleaned-on-kill-mixin
              #:add-groups-mixin

              #:group
              #:.sprites

              #:ordered-group

              #:group-single
              #:.sprite))

(defpackage #:lgame.font
  (:use #:common-lisp)
  (:documentation
    "Wrapper around sdl2-ttf, loads (and caches) fonts, provides a default font, and a render-text function that returns
     a texture.")
     (:export #:load-font
              #:unload-fonts
              #:render-text
              #:get-default-font
              #:get-default-mono-font
              #:find-font-path))

(defpackage #:lgame.render
  (:use #:common-lisp)
  (:documentation
    "Provides some wrappers around common sdl-render* calls.")
  (:export #:clear
           #:blit
           #:present
           #:set-draw-color
           #:with-draw-color
           #:with-render-target))

(defpackage #:lgame.draw
  (:use #:common-lisp)
  (:import-from #:lgame.rect
                #:with-rect)
  (:documentation
    "Provides additional drawing routines, like circles or rounded corner rects.")
  (:export #:render-fill-circle
           #:render-rounded-filled-rect))

(defpackage #:lgame.loader
  (:use #:common-lisp)
  (:documentation
    "Provides loader wrappers for various assets (currently just *texture-loader*) that may be easier
     than calling the underlying functions directly.")
     (:export #:get-texture-rect
              #:load-texture

              #:texture-loader
              #:.textures
              #:.default-dir

              #:create-texture-loader
              #:get-texture
              #:unload-textures))

(defpackage #:lgame.pathfinding
  (:use #:common-lisp)
  (:documentation
    "Provides a fairly generic A* algorithm protocol for grids with several customization knobs.")
  (:export #:A*
           #:.size
           #:.start-pos
           #:.end-pos
           #:.neighbor-fn
           #:.heuristic
           #:.heuristic-weight
           #:.waypoint-list
           #:.open-list
           #:.visited-list
           #:.parent-list
           #:a-star

           #:euclidean
           #:octile
           #:chebyshev
           #:manhattan

           #:compute-path))

(defpackage #:lgame.util
  (:use #:common-lisp)
  (:documentation
    "Utility functions that don't fit in other packages, like printing out license information.")
  (:export #:display-licenses))
