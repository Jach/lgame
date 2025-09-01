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
                :type (or null sdl2-ffi:sdl-texture))

   (%access-count :accessor .access-count
                  :initform 0)
   (%destroyed? :accessor .destroyed?
                :initform nil))

  (:documentation "A wrapper around sdl's texture. Represents a 2D grid of
                   pixel data. Until a thread-safe finalizer is made, textures
                   created in any way besides through the texture-loader must
                   have their underlying sdl-texture data freed manually with
                   DESTROY-TEXTURE.

                   Failure to destroy a texture leads to memory not being
                   released, but trying to use an already-destroyed texture
                   will result in a warning being signaled with some extra
                   information about how many times the underlying sdl-texture
                   was accessed.

                   When creating a new texture instance, if sdl-texture is
                   passed but width and height are not, the width and height
                   will be automatically calculated and set."))

(defmethod initialize-instance :after ((self texture) &key)
  (when (and (slot-value self 'sdl-texture) (zerop (.width self)) (zerop (.height self)))
    (setf (.width self) (sdl2:texture-width (slot-value self 'sdl-texture))
          (.height self) (sdl2:texture-height (slot-value self 'sdl-texture)))))

(defmethod .sdl-texture :after ((self texture))
  (incf (.access-count self)))

(defmethod destroy-texture ((self texture))
  "Attempts to destroy the underlying sdl-texture.
   Signals a warning if it has already been freed and returns nil.
   If it is successfully destroyed, returns t."
  (when (slot-value self 'sdl-texture)
    (if (.destroyed? self)
        (signal (make-condition 'warning :format-control "Trying to destroy an already-destroyed texture (accessed ~:R times), skipping"
                                         :format-arguments (list (.access-count self))))
        (progn
          (sdl2:destroy-texture (slot-value self 'sdl-texture))
          (setf (.destroyed? self) t)))))


(defun create-empty-sdl-texture (renderer access width height &optional (pixel-format lgame::+sdl-pixelformat-rgba8888+))
  "Creates a new lgame.texture:texture object with a backing sdl-texture
   created by calling sdl2:create-texture with these function arguments."
  (make-instance 'texture :sdl-texture (sdl2:create-texture renderer pixel-format access width height)))
