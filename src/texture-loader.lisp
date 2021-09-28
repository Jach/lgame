(in-package #:lgame.loader)

(annot:enable-annot-syntax)

(declaim (inline get-texture-rect))
@export
(defun get-texture-rect (texture)
  ; this is done since while the function belongs to rect proper,
  ; it's easy to mess up since texture loading and rect-getting often
  ; happen next to each other. Maybe later there'll be a function to
  ; get an image and rect at the same time.
  (lgame.rect:get-texture-rect texture))

@export
(defun load-texture (path-or-file &key color-key)
  "Simple texture loader from a path-or-file pointing to an image on disk,
   using the default *renderer*.
   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.
   Image types are those supported by sdl2-image's init, by default JPGs and PNGs.
   The caller is responsible for freeing the returned SDL_Texture."
  (let ((surface (sdl2-image:load-image (namestring path-or-file))))
    (when color-key
      (lgame::sdl-set-color-key surface 1 (apply #'lgame::sdl-map-rgb (sdl2:surface-format surface) color-key)))
    (let ((texture (sdl2:create-texture-from-surface lgame:*renderer* surface)))
      (lgame::sdl-free-surface surface)
      texture)))

@export-class
(defclass texture-loader ()
  ((textures :accessor .textures :initform (make-hash-table :test #'equal))
   (default-dir :accessor .default-dir :initarg :default-dir)))

@export
(defun create-texture-loader (default-dir)
  "Factory for a default lgame:*texture-loader*."
  (setf lgame.state:*texture-loader* (make-instance 'texture-loader :default-dir default-dir)))

@export
(defun get-texture (key-or-name &key dir color-key)
  "Get a texture using the default *texture-loader*
   Assumes the texture-loader has been previously created"
  (get-texture-internal lgame.state:*texture-loader* key-or-name :dir dir :color-key color-key))

(defmethod get-texture-internal ((self texture-loader) key-or-name &key dir color-key)
  "Load asset specified by, if given a keyword, converting it to a lowercase png file name inside dir,
   Otherwise expects a namestring of a file relative to the dir.
   If dir is not given, uses the default-dir of the texture loader.
   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.
   Returns an SDL_Texture.
   If the texture has already been loaded, it will return the texture from cache.
   All loaded textures can be freed and unloaded by calling unload-textures, which is done
   by default in lgame:quit if *texture-loader* has been bound."
  (when (null dir)
    (setf dir (.default-dir self)))
  (alexandria:if-let ((texture (gethash key-or-name (.textures self))))
    (if (autowrap:valid-p texture)
        texture
        (progn ; something invalidated it, force a reload
          (setf (gethash key-or-name (.textures self)) nil)
          (get-texture self key-or-name :dir dir :color-key color-key)))

    (let* ((filename (format nil "~a/~a" dir (if (keywordp key-or-name)
                                                 (uiop:strcat (string-downcase key-or-name) ".png")
                                                 key-or-name)))
           (texture (load-texture filename :color-key color-key)))
      (setf (gethash key-or-name (.textures self)) texture)
      texture)))

@export
(defmethod unload-textures ((self texture-loader))
  (maphash (lambda (key val)
             (declare (ignore key))
             (if (autowrap:valid-p val)
               (lgame::sdl-destroy-texture val)))
           (.textures self))
  (clrhash (.textures self)))

