(in-package #:lgame.render)

(annot:enable-annot-syntax)

(declaim (inline clear))
@export
(defun clear ()
  "Wrapper around sdl-render-clear."
  (lgame::sdl-render-clear lgame:*renderer*))

(declaim (inline blit))
@export
(defun blit (texture destination-rect)
  "Wrapper around sdl-render-copy for the common case of 'blitting' a texture to
   a destination-rect on the screen."
  (lgame::sdl-render-copy lgame:*renderer* texture nil destination-rect))

(declaim (inline present))
@export
(defun present ()
  "Wrapper around sdl-render-present."
  (lgame::sdl-render-present lgame:*renderer*))

@export
(defun set-draw-color (&optional (color-or-r 0) (g 0) (b 0) (a 255))
  "Sets the current render-draw color.
   Can pass rgba as individual arguments, or a single color list as the
   one and only first argument. If the color list only has three elements
   for rgb, the alpha 255 is used."
  (if (typep color-or-r 'list)
      (apply #'lgame::sdl-set-render-draw-color lgame:*renderer* (if (= 4 (length color-or-r))
                                                                     color-or-r
                                                                     (append color-or-r '(255))))
      (lgame::sdl-set-render-draw-color lgame:*renderer* color-or-r g b a)))

@export
(defmacro with-draw-color ((&optional (color-or-r 0) (g 0) (b 0) (a 255)) &body body)
  (let ((current (gensym)))
    `(let ((,current (multiple-value-list (sdl2:get-render-draw-color lgame.state:*renderer*))))
       (set-draw-color ,color-or-r ,g ,b ,a)
       ,@body
       (set-draw-color ,current))))

@export
(defmacro with-render-target (target-texture &body body)
  `(progn
     (sdl2:set-render-target lgame:*renderer* ,target-texture)
     ,@body
     (sdl2:set-render-target lgame:*renderer* nil)))
