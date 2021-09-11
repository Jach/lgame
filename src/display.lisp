(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defun create-window (title x y w h &optional (flags +sdl-window-opengl+))
  "Wrapper around sdl-create-window, binds to lgame:*screen* and returns it.
   Also sets lgame:*screen-rect* to use the window width and height."
  (setf *screen* (sdl-create-window title x y w h flags))
  (if (null-ptr? *screen*)
      (error 'sdl-error :msg (sdl-get-error)))
  (setf *screen-rect* (sdl2:make-rect 0 0 w h))
  *screen*)

@export
(defun create-renderer (&optional (window *screen*) (index -1) (flags 0))
  "Wrapper around sdl-create-renderer, binds to lgame:*renderer* and returns it.
   Default values are lgame:*screen*, default monitor, and no special flags."
  (setf *renderer* (sdl-create-renderer window -1 0))
  (if (null-ptr? *renderer*)
      (error 'sdl-error :msg (sdl-get-error)))
  *renderer*)

@export
(defun set-logical-size (w h &optional (renderer *renderer*))
  "Mostly a wrapper around sdl-render-set-logical-size.
   By default sets the render scaling quality to 'linear',
   and resets lgame:*screen-rect* to use the new logical width and height, returning it."
  (sdl-set-hint +sdl-hint-render-scale-quality+ "linear")
  (unless (zerop
            (sdl-render-set-logical-size renderer w h))
    (error 'sdl-error :msg (sdl-get-error)))
  (if *screen-rect*
      (sdl2:free-rect *screen-rect*))
  (setf *screen-rect* (sdl2:make-rect 0 0 w h)))
