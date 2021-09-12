
(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defun get-mouse-pos ()
  "maybe delete this.."
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (list x y)))

