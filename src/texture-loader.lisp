(in-package #:lgame)

(annot:enable-annot-syntax)

@export-class
(defclass texture-loader ()
  ((textures :accessor textures-of :initform (make-hash-table))
   (default-dir :accessor default-dir-of :initarg :default-dir)))

@export
(defmethod load-texture ((self texture-loader) key &optional dir)
  "Load asset specified by converting given key to lowercase png file name inside dir,
   returning an SDL_Texture.
   If the key'd texture has already been loaded, it will return the texture from cache."
  (when (null dir)
    (setf dir (default-dir-of self)))
  (alexandria:if-let ((texture (gethash key (textures-of self))))
    texture
    (let* ((filename (format nil "~a/~a.png" dir (string-downcase key)))
           (surface (sdl2-image:load-image filename))
           (texture (sdl-create-texture-from-surface *renderer* surface)))
      (sdl-free-surface surface)
      (setf (gethash key (textures-of self)) texture)
      texture)))

@export
(defmethod unload-textures ((self texture-loader))
  (maphash (lambda (key val)
             (declare (ignore key))
             (sdl-destroy-texture val))
           (textures-of self)))
