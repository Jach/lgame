(in-package #:lgame.mouse)

(annot:enable-annot-syntax)

@export
(defun get-mouse-pos ()
  "Simple wrapper around 'sdl2:mouse-state, returns the current mouse's position
   as an x,y list.
   Note that this is the position relative to the actual window size, NOT the
   logical render size. If you need the latter, consider getting the :button :x and :y
   values from the mouse event, which should be relative to the logical size."
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (list x y)))

