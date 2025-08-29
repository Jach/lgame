(in-package #:lgame.texture)

(defclass texture ()
  ((width :accessor .width
          :initarg :width
          :initform 0)
   (height :accessor .height
           :initarg :height
           :initform 0)
   (sdl-texture :accessor .sdl-texture
                :initarg :sdl-texture
                :type (or null sdl2-ffi:sdl-texture)))
  (:documentation "A wrapper around sdl's texture. Represents a 2D grid of pixel data.
                   Until a thread-safe finalizer is made, textures created in any way besides through the texture-loader must
                   have their underlying sdl-texture data freed manually with destroy-texture.
                   When creating a new texture instance, if sdl-texture is passed but width and height are not, the width and height will
                   be automatically calculated and set."))

(defmethod initialize-instance :after ((self texture) &key)
  (when (and (.sdl-texture self) (zerop (.width self)) (zerop (.height self)))
    (setf (.width self) (sdl2:texture-width (.sdl-texture self))
          (.height self) (sdl2:texture-height (.sdl-texture self)))))

(defmethod destroy-texture ((self texture))
  (when (.sdl-texture self)
    (sdl2:destroy-texture (.sdl-texture self))))

(defun create-sdl-texture (renderer pixel-format access width height)
  "Creates a new lgame.texture:texture object with a backing sdl-texture
   created by calling sdl2:create-texture with these function arguments."
  (make-instance 'texture :sdl-texture (sdl2:create-texture renderer pixel-format access width height)))
