#|
Simple example of using SDL2 with OpenGL.
The key differences are:
1) In init, after creating a window lgame:*screen* and renderer lgame:*renderer*,
initialize a GL context.
  1a) Bind sdl-gl-get-proc-address to cl-opengl-bindings:*gl-get-proc-address* so that cl-opengl can find opengl extensions
2) In game-tick, use gl: functions to clear the screen etc. and gl-swap-window instead of render-present.

After loading this up, you can try following along with http://3bb.cc/tutorials/cl-opengl/index.html
Code to draw a colorful triangle is commented out before the window swap.
|#

;; quicklisp preamble and quickloading for script usage
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame)
  (ql:quickload :livesupport)
  (ql:quickload :cl-opengl)) ;; WARNING: the version of cl-opengl in quicklisp might be broken, consider downloading latest from https://github.com/3b/cl-opengl/

(defpackage #:lgame.example.gl
  (:use #:cl)
  (:export #:main))
(in-package #:lgame.example.gl)

(defvar *gl-context* nil)

(defun init ()
  (lgame:init)
  (lgame.display:create-centered-window "GL" 800 600 lgame::+sdl-window-opengl+)
  (lgame.display:create-renderer)

  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2-ffi.functions:sdl-gl-get-proc-address)
  (setf *gl-context* (lgame::sdl-gl-create-context lgame:*screen*)))

(defun game-loop ()
  (lgame.time:clock-start)
  (loop while (lgame.time:clock-running?) do
        (livesupport:continuable
          (game-tick))))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+))
      (lgame.time:clock-stop)))

  ;; For old fashioned gl, instead of:
  ;(sdl2:set-render-draw-color lgame:*renderer* 255 0 255 255)
  ;(sdl2:render-clear lgame:*renderer*)
  ;(sdl2:render-present lgame:*renderer*)
  ;; We do:

  ; Call functions in gl: and work with the *screen* instead.
  (gl:clear-color 1.0 0 1 1)
  (gl:clear :color-buffer-bit)

  ;; Add a triangle
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0)
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0))
  (gl:flush)
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
