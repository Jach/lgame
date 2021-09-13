(in-package #:lgame.mouse)

(annot:enable-annot-syntax)

@export
(defun get-mouse-pos ()
  "Simple wrapper around 'sdl2:mouse-state, returns the current mouse's position
   as an x,y list."
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (list x y)))

