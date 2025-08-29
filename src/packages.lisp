(in-package #:cl-user)

(defpackage #:lgame.texture
  (:use #:cl)
  (:documentation
    "Provides a wrapper around an SDL texture. This is done to potentially simplify foreign resource management in the future, or move to a different backing library than SDL.")
  (:export #:texture
           #:.width
           #:.height
           #:.sdl-texture
           #:destroy-texture
           #:create-sdl-texture))

(defpackage #:lgame.box
  (:use #:common-lisp)
  (:documentation
    "2D bounding boxes (rects) and helpful utilities like some collision detection tests.
     Meant to replace uses of SDL_Rect.")
     (:export #:box
              #:make-box
              #:make-box-from-minmax
              #:copy-box

              #:box-x
              #:box-y
              #:box-width
              #:box-height

              #:box-min-x
              #:box-min-y
              #:box-max-x
              #:box-max-y

              #:with-box-as-sdl-rect
              #:get-texture-box

              #:box-attr

              #:box-contains-point?
              #:boxes-intersect?
              #:boxes-collide?
              #:box-contains?
              #:box-properly-contains?

              #:move-box
              #:set-box
              #:clamp
              #:inflate-box

              #:with-moved-box))

(defpackage #:lgame.state
  (:use #:common-lisp)
  (:documentation
    "This package is to manage state shared among the various lgame modules and a game.
     lgame is currently designed to only handle one game at a time, so there is typically
     just one window/screen, one renderer, one resource manager for each resource type...")
  (:export #:*screen*
           #:*screen-rect*
           #:*screen-box*
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
           #:*screen-box*
           #:*renderer*
           #:*texture-loader*
           #:lgame-error
           #:null-ptr?))

(defpackage #:lgame.data-structures
  (:use #:cl)
  (:documentation
    "Various data structures implemented for supporting other packages but general enough to
     potentially be useful elsewhere.")
  (:export #:priority-queue
           #:priority-queue-push
           #:priority-queue-pop
           #:priority-queue-top
           #:priority-queue-empty?))


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
           #:now-seconds
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
    "Deprecated: use lgame.box.
     Utils around handling SDL_Rects. Several with- macros like 'with-rect are included to efficiently
     use a stack-allocated object instead of allocating and freeing, though for any longer-lasting rects
     one should use the usual 'sdl2:make-rect and 'free-rect. Also included are some intuitive lookup/setter functions,
     and collision detection routines."))

(defpackage #:lgame.sprite
  (:use #:common-lisp)
  (:documentation
    "WIP sprite module, inspired by pygame.sprite. The idea is to provide a base Sprite
     class that sprite objects can inherit from and optionally override their own update
     and draw methods to use the image/box slots differently. Additionally a Group class
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
              #:.box
              #:.angle
              #:.flip
              #:.alive?
              #:.groups

              #:add-groups-mixin

              #:group
              #:.sprites

              #:ordered-group

              #:group-single
              #:.sprite))

(defpackage #:lgame.font.ffi
  (:use #:cl)
  (:documentation
    "Foreign library and c function definitions for font rendering.
     Currently using sdl2-ttf, only implementing the bare minimum.")
  (:export #:ttf-init
           #:ttf-quit
           #:ttf-open-font
           #:ttf-close-font
           #:ttf-render-utf8-blended))

(defpackage #:lgame.font
  (:use #:common-lisp)
  (:documentation
    "Provides higher level font rendering utilities, like loading (and caching) fonts,
     a default font path, and a render-text function that returns a texture.")
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
     (:export #:load-texture

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
           #:a-star

           #:compute-path
           #:found-shortest-path))

(defpackage #:lgame.util
  (:use #:common-lisp)
  (:documentation
    "Utility functions that don't fit in other packages, like printing out license information.")
  (:export #:display-licenses))
