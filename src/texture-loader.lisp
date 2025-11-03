(in-package #:lgame.loader)

(defun load-texture (absolute-path-or-file &key color-key)
  "Always returns a new texture made from a absolute-path-or-file name pointing to an image on disk.
   Uses the default *renderer*, wraps the underlying object within a lgame.texture:texture object.

   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.

   Image file types are those supported by sdl2-image's init, by default JPGs and PNGs.
   The caller is responsible for freeing the returned SDL_Texture."
  (let ((surface (sdl2-image:load-image (namestring absolute-path-or-file))))
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

(defmethod get-texture-internal ((self texture-loader) key-or-name &key dir color-key skip-caching?)
  "Load asset specified by, if given a keyword, converting it to a lowercase png file name inside dir,
   Otherwise expects a namestring of a file relative to the dir.

   If dir is not given, uses the default-dir of the texture loader.

   An optional color-key may be given, which is a list of R G B, that will be used to set the color key of the loaded image.

   Returns a lgame.texture:texture that currently wraps an SDL_Texture.

   If the texture has already been loaded, it will return the texture from cache.

   If skip-caching? is set to t, then the loaded texture will not be stored in the cache,
   and this behaves similarly to the load-texture function.

   All loaded textures can be freed and unloaded by calling unload-textures, which is done
   by default in lgame:quit if *texture-loader* has been set."
  (when (null dir)
    (setf dir (.default-dir self)))
  (alexandria:if-let ((texture (gethash key-or-name (.textures self))))
    (if (valid-texture-or-array texture) ; get it from cache
        texture
        (progn ; something invalidated it, force a reload
          (setf (gethash key-or-name (.textures self)) nil)
          (get-texture-internal self key-or-name :dir dir :color-key color-key)))

    (let* ((filename (format nil "~a/~a" dir (if (keywordp key-or-name)
                                                 (uiop:strcat (string-downcase key-or-name) ".png")
                                                 key-or-name)))
           (texture (load-texture filename :color-key color-key)))
      (unless skip-caching?
        (setf (gethash key-or-name (.textures self)) texture))
      texture)))

(defun valid-texture-or-array (texture)
  (if (arrayp texture)
      (every #'if-valid-texture-or-array texture)
      (autowrap:valid-p (lgame.texture:.sdl-texture texture))))

(defmethod unload-textures ((self texture-loader))
  (maphash (lambda (key val)
             (declare (ignore key))
             (if (arrayp val)
                 (loop for frame across val do (lgame.texture:destroy-texture frame))
                 (lgame.texture:destroy-texture val)))
           (.textures self))
  (clrhash (.textures self)))




(defun get-texture-frames-from-horizontal-strip (key-or-name &key (frame-width 0) (offset 0) (estimate-width? nil) dir color-key)
  "Given a key-or-name path that represents a horizontal strip of sprite animation frames, each of
   a certain frame-width and offset from each other by frame-width + offset, return an array containing
   the frames as individual textures in order. If color-key is given, it is applied to each frame.

   Example strip that has 0 pixels of extra offset/border between frames: https://opengameart.org/content/simple-rotating-coin

   If frame-width is left as the default of 0, then estimate-width? is considered. If it is also left as the default of nil,
   the width is assumed to be equal to the height -- i.e. each sprite frame is a square.
   If estimate-width? is set to t, then currently there's no difference, but in the future we may try to apply some sort of
   heuristic to make a best-guess at the width and go with that.

   Like textures fetched via get-texture, these textures are automatically cached -- just the frames though, not the original whole strip --
   so repeat calls will return the same array."
  (declare (ignorable estimate-width?))
  (let* ((texture (get-texture-internal lgame.state:*texture-loader* key-or-name :dir dir :color-key color-key :skip-caching? t))
         (frame-height (lgame.texture:.height texture)))
    (when (zerop frame-width)
      (setf frame-width frame-height))
    (let* ((frame-count (truncate (/ (lgame.texture:.width texture) frame-width)))
           (frames (make-array frame-count :fill-pointer 0))
           (clip-src (lgame.box:make-box 0 0 frame-width frame-height)))
      (loop for i below frame-count do
            (let ((frame-texture (lgame.texture:create-empty-sdl-texture lgame.state:*renderer* lgame::+sdl-textureaccess-target+
                                                                         frame-width frame-height)))
              (lgame.render:with-render-target frame-texture
                (lgame.render:blit texture nil clip-src))
              (vector-push frame-texture frames)
              (lgame.box:move-box clip-src (+ frame-width offset) 0)))
      (setf (gethash key-or-name (.textures lgame.state:*texture-loader*)) frames)
      (lgame.texture:destroy-texture texture)
      frames)))
