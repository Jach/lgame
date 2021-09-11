(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defun load-texture (path-or-file)
  "Simple texture loader from a path-or-file pointing to an image on disk,
   using the default *renderer*.
   Image types are those supported by sdl2-image's init, by default JPGs and PNGs.
   The caller is responsible for freeing the returned SDL_Texture."
  (let* ((surface (sdl2-image:load-image (namestring path-or-file)))
         (texture (sdl-create-texture-from-surface *renderer* surface)))
    (sdl-free-surface surface)
    texture))

@export-class
(defclass texture-loader ()
  ((textures :accessor textures-of :initform (make-hash-table))
   (default-dir :accessor default-dir-of :initarg :default-dir)))

@export
(defun create-texture-loader (default-dir)
  "Factory for a default lgame:*texture-loader*."
  (setf *texture-loader* (make-instance 'texture-loader :default-dir default-dir)))

@export
(defmethod get-texture ((self texture-loader) key-or-name &optional dir)
  "Load asset specified by, if given a keyword, converting it to a lowercase png file name inside dir,
   Otherwise expects a namestring of a file relative to the dir.
   Returns an SDL_Texture.
   If the texture has already been loaded, it will return the texture from cache.
   All loaded textures can be freed and unloaded by calling unload-textures, which is done
   by default in lgame:quit if *texture-loader* has been bound."
  (when (null dir)
    (setf dir (default-dir-of self)))
  (alexandria:if-let ((texture (gethash key-or-name (textures-of self))))
    texture
    (let* ((filename (format nil "~a/~a" dir (if (keywordp key-or-name)
                                                 (uiop:strcat (string-downcase key-or-name) ".png")
                                                 key-or-name)))
           (texture (load-texture filename)))
      (setf (gethash key-or-name (textures-of self)) texture)
      texture)))

@export
(defmethod unload-textures ((self texture-loader))
  (maphash (lambda (key val)
             (declare (ignore key))
             (sdl-destroy-texture val))
           (textures-of self))
  (clrhash (textures-of self)))

