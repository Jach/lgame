(in-package #:lgame.mouse)

(annot:enable-annot-syntax)

(defvar *hover-cursor* nil)
(defvar *normal-cursor* nil)

@export
(defun get-mouse-pos ()
  "Simple wrapper around 'sdl2:mouse-state, returns the current mouse's position
   as an x,y list.
   Note that this is the position relative to the actual window size, NOT the
   logical render size. If you need the latter, consider getting the :button :x and :y
   values from the mouse event, which should be relative to the logical size."
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (list x y)))

@export
(defun set-hover-cursor ()
  (unless *hover-cursor*
    (setf *hover-cursor* (lgame::sdl-create-system-cursor lgame::+sdl-system-cursor-hand+)))
  (lgame::sdl-set-cursor *hover-cursor*))

@export
(defun set-normal-cursor ()
  (unless *normal-cursor*
    (setf *normal-cursor* (lgame::sdl-create-system-cursor lgame::+sdl-system-cursor-arrow+)))
  (lgame::sdl-set-cursor *normal-cursor*))

@export
(defun cleanup-cursors ()
  (when *hover-cursor*
    (lgame::sdl-free-cursor *hover-cursor*))
  (when *normal-cursor*
    (lgame::sdl-free-cursor *normal-cursor*)))

