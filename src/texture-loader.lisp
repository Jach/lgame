(in-package #:lgame.loader)

(defun load-texture (path-or-file &key color-key)
  "Simple texture loader from a path-or-file pointing to an image on disk,
   using the default *renderer*, returns a wrapped texture within an lgame.texture:texture object.

   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.

   Image file types are those supported by sdl2-image's init, by default JPGs and PNGs.
   The caller is responsible for freeing the returned SDL_Texture."
  (let ((surface (sdl2-image:load-image (namestring path-or-file))))
    (when color-key
      (lgame::sdl-set-color-key surface 1 (apply #'lgame::sdl-map-rgb (sdl2:surface-format surface) color-key)))
    (let ((texture (sdl2:create-texture-from-surface lgame:*renderer* surface)))
      (lgame::sdl-free-surface surface)
      (make-instance 'lgame.texture:texture :sdl-texture texture :width (sdl2:texture-width texture) :height (sdl2:texture-height texture)))))

(defclass texture-loader ()
  ((textures :accessor .textures :initform (make-hash-table :test #'equal))
   (default-dir :accessor .default-dir :initarg :default-dir)))

(defun create-texture-loader (default-dir)
  "Factory for a default lgame:*texture-loader*."
  (setf lgame.state:*texture-loader* (make-instance 'texture-loader :default-dir default-dir)))

(defun get-texture (key-or-name &key dir color-key)
  "Get a texture using the default *texture-loader*
   Assumes the texture-loader has been previously created"
  (get-texture-internal lgame.state:*texture-loader* key-or-name :dir dir :color-key color-key))

(defmethod get-texture-internal ((self texture-loader) key-or-name &key dir color-key)
  "Load asset specified by, if given a keyword, converting it to a lowercase png file name inside dir,
   Otherwise expects a namestring of a file relative to the dir.

   If dir is not given, uses the default-dir of the texture loader.

   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.

   Returns a lgame.texture:texture that currently wraps an SDL_Texture.

   If the texture has already been loaded, it will return the texture from cache.

   All loaded textures can be freed and unloaded by calling unload-textures, which is done
   by default in lgame:quit if *texture-loader* has been set."
  (when (null dir)
    (setf dir (.default-dir self)))
  (alexandria:if-let ((texture (gethash key-or-name (.textures self))))
    (if (autowrap:valid-p (lgame.texture:.sdl-texture texture)) ; get it from cache
        texture
        (progn ; something invalidated it, force a reload
          (setf (gethash key-or-name (.textures self)) nil)
          (get-texture-internal self key-or-name :dir dir :color-key color-key)))

    (let* ((filename (format nil "~a/~a" dir (if (keywordp key-or-name)
                                                 (uiop:strcat (string-downcase key-or-name) ".png")
                                                 key-or-name)))
           (texture (load-texture filename :color-key color-key)))
      (setf (gethash key-or-name (.textures self)) texture)
      texture)))

(defmethod unload-textures ((self texture-loader))
  (maphash (lambda (key val)
             (declare (ignore key))
             (when (autowrap:valid-p (lgame.texture:.sdl-texture val))
               (sdl2:destroy-texture (lgame.texture:.sdl-texture val))))
           (.textures self))
  (clrhash (.textures self)))

