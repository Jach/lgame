#|
Simple example of using SDL2 with OpenGL.
The key differences are:
1) In init, after creating a window (lgame:*screen*) and renderer (lgame:*renderer*),
  initialize a GL context.
  1a) Bind sdl-gl-get-proc-address to cl-opengl-bindings:*gl-get-proc-address* so that cl-opengl can find opengl extensions
2) In game-tick, use gl: functions to clear the screen etc. and gl-swap-window instead of render-present.

After loading this up, you can try following along with http://3bb.cc/tutorials/cl-opengl/index.html
Code to draw a colorful triangle is commented out before the window swap.
|#

(ql:quickload :lgame)
(ql:quickload :livesupport)
(ql:quickload :cl-opengl)

(defpackage #:gl-ex
  (:use :cl))
(in-package #:gl-ex)

(defvar *running?* t)

(defvar *gl-context* nil)

(defun init ()
  (lgame:init)
  (lgame.display:create-window "GL" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+
                               800 600)
  (lgame.display:create-renderer)

  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2-ffi.functions:sdl-gl-get-proc-address)
  (setf *gl-context* (lgame::sdl-gl-create-context lgame:*screen*))
  )

(defun game-loop ()
  (lgame.time:clock-start)
  (setf *running?* t)
  (loop while *running?* do
        (livesupport:continuable
          (game-tick))))

(defun game-tick ()
  (lgame.event:do-event (event)
    (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+))
        (setf *running?* nil)))
  ; instead of:
  ;(sdl2:set-render-draw-color lgame:*renderer* 255 0 255 255)
  ;(sdl2:render-clear lgame:*renderer*)
  ;(sdl2:render-present lgame:*renderer*)
  (gl:clear-color 1.0 0 1 1)
  (gl:clear :color-buffer-bit)

  #|
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0)
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0))
  (gl:flush)
  |#
  (sdl2:gl-swap-window lgame:*screen*)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick 60))

(defun main ()
  (unwind-protect
    (progn
      (init)
      (game-loop))

    (lgame:quit)))

(eval-when (:execute)
  (main))
