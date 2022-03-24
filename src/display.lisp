(in-package #:lgame.display)

(defun create-window (title w h &optional (x lgame::+sdl-windowpos-undefined+) (y lgame::+sdl-windowpos-undefined+) (flags lgame::+sdl-window-shown+))
  "Wrapper around sdl-create-window, binds to lgame:*screen* and returns it.
   Also sets lgame:*screen-rect* to use the window width and height."
  (setf lgame.state:*screen* (lgame::sdl-create-window title x y w h flags))
  (if (lgame::null-ptr? lgame:*screen*)
      (error 'sdl-error :msg (lgame::sdl-get-error)))
  (setf lgame.state:*screen-rect* (sdl2:make-rect 0 0 w h))
  lgame.state:*screen*)

(defun create-centered-window (title w h &optional (flags lgame::+sdl-window-shown+))
  (create-window title w h lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ flags))

(defun create-renderer (&optional (window lgame.state:*screen*) (index -1) (flags 0))
  "Wrapper around sdl-create-renderer, binds to lgame:*renderer* and returns it.
   Default values are lgame:*screen*, default monitor, and no special flags."
  (setf lgame.state:*renderer* (lgame::sdl-create-renderer window index flags))
  (if (lgame::null-ptr? lgame.state:*renderer*)
      (error 'sdl-error :msg (lgame::sdl-get-error)))
  lgame.state:*renderer*)

(defun set-logical-size (w h &optional (renderer lgame.state:*renderer*) (linear-scaling? T))
  "Mostly a wrapper around sdl-render-set-logical-size.
   By default sets the render scaling quality to 'linear',
   and resets lgame:*screen-rect* to use the new logical width and height, returning it."
  (if linear-scaling?
      (lgame::sdl-set-hint lgame::+sdl-hint-render-scale-quality+ "linear"))
  (unless (zerop
            (lgame::sdl-render-set-logical-size renderer w h))
    (error 'sdl-error :msg (lgame::sdl-get-error)))
  (when lgame.state:*screen-rect*
      (sdl2:free-rect lgame.state:*screen-rect*))
  (setf lgame.state:*screen-rect* (sdl2:make-rect 0 0 w h)))

(defun screenshot-png (path-namestring)
  "Saves the current state of the renderer
   to a PNG image."
  (let ((surface (sdl2:create-rgb-surface (sdl2:rect-width lgame.state:*screen-rect*) (sdl2:rect-height lgame.state:*screen-rect*) 32)))
    (sdl2-ffi.functions:sdl-render-read-pixels lgame.state:*renderer* nil lgame::+sdl-pixelformat-argb8888+
                                               (plus-c:c-ref surface sdl2-ffi:sdl-surface :pixels)
                                               (plus-c:c-ref surface sdl2-ffi:sdl-surface :pitch))
    (sdl2-image:save-png surface path-namestring)
    (sdl2:free-surface surface)))
