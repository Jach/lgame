(in-package #:lgame)

(annot:enable-annot-syntax)

@export-structure
(defstruct velocity
  (dx 0)
  (dy 0))

@export
(defun velocity-reverse-dx (vel)
  (setf (velocity-dx vel) (* -1 (velocity-dx vel))))

@export
(defun velocity-reverse-dy (vel)
  (setf (velocity-dy vel) (* -1 (velocity-dy vel))))
