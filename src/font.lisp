(in-package #:lgame.font)

(defvar *rendered-texts-count* 0)
(defvar *rendered-text-textures* (trivial-garbage:make-weak-hash-table :weakness :value))
(defvar *successful-destroys* 0)

(defun destroy-rendered-text-textures ()
  "Called automatically by lgame:quit, any textures still reachable
   through the weak hash map that haven't been destroyed will be destroyed."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (and (not (lgame.texture::.destroyed? v))
                        (lgame.texture:destroy-texture v))
               (incf *successful-destroys*)))
           *rendered-text-textures*)
  (clrhash *rendered-text-textures*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *loaded-fonts* (make-hash-table :test #'equal))

  (defun unload-fonts ()
    (maphash (lambda (k v)
               (declare (ignore k))
               (lgame.font.ffi:ttf-close-font v))
             *loaded-fonts*)
    (clrhash *loaded-fonts*))

  (when (plusp (hash-table-count *loaded-fonts*)) ; need to wipe cache and reinit ttf to avoid invalid memory errors...
    (unload-fonts)
    (lgame.font.ffi:ttf-quit)
    (lgame.font.ffi:ttf-init)))

(defun load-font (font-path pt-size)
  "Loads a font at the specified size.
   If font-path is not a path or isn't a valid file, will attempt
   to treat it as naming a font-family, and will use
   #'find-font-path to attempt to resolve that into a path."

  (multiple-value-bind (font present?) (gethash (cons (namestring font-path) pt-size) *loaded-fonts*)
    (unless present?

      (let ((true-path font-path))
        (unless (uiop:file-exists-p font-path)
          (if (equal (type-of font-path) 'pathname) ; attempted to use a path but doesn't exist, use default
              (setf true-path (get-default-font))
              (setf true-path (find-font-path font-path)))) ; otherwise if it was a string, try to delegate

        (setf font (lgame.font.ffi:ttf-open-font (namestring true-path) pt-size))

        (if (lgame:null-ptr? font)
            (restart-case (error 'lgame:lgame-error :msg (lgame::sdl-get-error))
              (use-default-font () (return-from load-font (load-font (get-default-font) pt-size)))))

        (setf (gethash (cons (namestring font-path) pt-size) *loaded-fonts*) font)))

    font))

(defun pack-sdl-color (r g b a)
  "To avoid a dependency on libffi, which was used for being able to pass this sdl-color struct by value,
   we instead create an integer-packed color that's bitwise identical.
   https://wiki.libsdl.org/SDL2/SDL_Color
   "
  (logior (ash r 0) (ash g 8) (ash b 16) (ash a 24)))

(defun render-text (font text r g b &optional (a 255))
  "Using the provided font, renders the text string with a specified rgba color.

   This calls the underlying ttf-render-utf8-blended function.

   An lgame.texture:texture is returned. You should destroy this texture
   manually, but if you know it won't be claimed by the garbage collector prior
   to lgame:quit, then it will be destroyed automatically during lgame:quit."
  (let ((c (pack-sdl-color r g b a)))
    (let* ((surf (lgame.font.ffi:ttf-render-utf8-blended font text c))
           (tex (make-instance 'lgame.texture:texture :sdl-texture (lgame::sdl-create-texture-from-surface lgame:*renderer* surf))))
      (lgame::sdl-free-surface surf)
      (setf (gethash *rendered-texts-count* *rendered-text-textures*) tex)
      (incf *rendered-texts-count*)
      tex)))

; Alternative/supplemental implemenations of render-text to consider:
; a render text that caches the texture instead of the weak hash table scheme, like texture loader
; a render text that takes a top-left coord to draw, or a box, and draws immediately (and destroys immediately or caches)
; a render text that takes an (r g b) or (r g b a) list instead of separate params

(defun get-default-font ()
  (asdf:system-relative-pathname :lgame "assets/open-sans/OpenSans-Regular.ttf"))

(defun get-default-mono-font ()
  (asdf:system-relative-pathname :lgame "assets/roboto-mono/RobotoMono-Regular.ttf"))

(defun find-font-path (font-family)
  "Using the font-discovery library, attempts to find a font
   named by the font-family and return its path for use by load-font.
   If the font cannot be found, the library uses a system default font like
   dejavu sans."
  (org.shirakumo.font-discovery:file
    (org.shirakumo.font-discovery:find-font :family font-family)))
