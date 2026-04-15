(in-package #:lgame.render)

(defun clear ()
  "Wrapper around sdl-render-clear."
  (lgame::sdl-render-clear lgame:*renderer*))

(defun blit (texture destination-box &optional (source-box nil))
  "Wrapper around sdl-render-copy for the common case of 'blitting' a texture to
   a destination-box on the screen.
   Expects an lgame.texture:texture object and handles conversion of the box to an sdl rect.
   An optional source box can be passed but is often not needed."

  (sdl2::check-rc
    (cond ((and destination-box source-box)
           (lgame.box:with-box-as-sdl-rect (dest-rect destination-box)
             (lgame.box:with-box-as-sdl-rect (source-rect source-box)
               (sdl2-ffi.functions:sdl-render-copy lgame:*renderer* (lgame.texture:.sdl-texture texture) source-rect dest-rect))))
          (destination-box
            (lgame.box:with-box-as-sdl-rect (dest-rect destination-box)
              (sdl2-ffi.functions:sdl-render-copy lgame:*renderer* (lgame.texture:.sdl-texture texture) nil dest-rect)))
          (source-box
            (lgame.box:with-box-as-sdl-rect (source-rect source-box)
              (sdl2-ffi.functions:sdl-render-copy lgame:*renderer* (lgame.texture:.sdl-texture texture) source-rect nil)))
          (t (sdl2-ffi.functions:sdl-render-copy lgame:*renderer* (lgame.texture:.sdl-texture texture) nil nil)))))

(defun present ()
  "Wrapper around sdl-render-present."
  (lgame::sdl-render-present lgame:*renderer*))

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

(defmacro with-draw-color ((&optional (color-or-r 0) (g 0) (b 0) (a 255)) &body body)
  (let ((current (gensym)))
    `(let ((,current (multiple-value-list (sdl2:get-render-draw-color lgame.state:*renderer*))))
       (set-draw-color ,color-or-r ,g ,b ,a)
       ,@body
       (set-draw-color ,current))))

(defmacro with-render-target (target-texture &body body)
  "Set the renderer target to the given target-texture for the context of body.
   target-texture is expected to be an lgame.texture:texture object."
  (let ((old-target (gensym)))
  `(let ((,old-target (sdl2:get-render-target lgame:*renderer*)))
     (unwind-protect
       (progn
         (sdl2:set-render-target lgame:*renderer* (lgame.texture:.sdl-texture ,target-texture))
         ,@body)
       (sdl2:set-render-target lgame:*renderer* ,old-target)))))

(defun set-texture-color-mod (texture &optional (color-or-r 255) (g 255) (b 255))
  "Sets an additional color value to multiply texture colors with on render according to:
   srcC = srcC * (color / 255)
   Expects an lgame.texture:texture as the first argument.
   Can pass rgb as individual arguments after, or as a single color list of 3 elements as the one and only first argument.
   Passing no color will result in no change."
  (let ((sdl-texture (lgame.texture:.sdl-texture texture)))
    (if (typep color-or-r 'list)
        (apply #'sdl2:set-texture-color-mod sdl-texture color-or-r)
        (sdl2:set-texture-color-mod sdl-texture color-or-r g b))))

(defmacro with-texture-color-mod ((texture &optional (color-or-r 255) (g 255) (b 255)) &body body)
  "Given an lgame.texture:texture and a color, allows rendering the texture with a color mod, then restoring the original color mod after.
   See SET-TEXTURE-MOD for more."
  (let ((current (gensym)))
    `(let ((,current (multiple-value-list (sdl2:get-texture-color-mod (lgame.texture:.sdl-texture ,texture)))))
       (set-texture-color-mod ,texture ,color-or-r ,g ,b)
       ,@body
       (set-texture-color-mod ,texture ,current))))
