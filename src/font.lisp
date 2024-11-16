(in-package #:lgame.font)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *loaded-fonts* (make-hash-table :test #'equal))

  (defun unload-fonts ()
    (maphash (lambda (k v)
               (declare (ignore k))
               (lgame-sdl2-ttf.ffi:ttf-close-font v))
             *loaded-fonts*)
    (clrhash *loaded-fonts*))

  (when (plusp (hash-table-count *loaded-fonts*)) ; need to wipe cache and reinit ttf to avoid invalid memory errors...
    (unload-fonts)
    (lgame-sdl2-ttf.ffi:ttf-quit)
    (lgame-sdl2-ttf.ffi:ttf-init)))

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

        (setf font (lgame-sdl2-ttf.ffi:ttf-open-font (namestring true-path) pt-size))

        (if (lgame:null-ptr? font)
            (restart-case (error 'lgame:lgame-error :msg (lgame::sdl-get-error))
              (use-default-font () (return-from load-font (load-font (get-default-font) pt-size)))))

        (setf (gethash (cons (namestring font-path) pt-size) *loaded-fonts*) font)))

    font))

(cffi:defcstruct sdl-color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defun render-text (font text r g b &optional (a 255))
  ; todo, add support for an optional cached? parameter.
  ; when t, and *texture-loader* has been created,
  ; the (font+text) key is used to cache the created texture.
  ; This way, a game can naively call render-text every frame
  ; and live-change it without having to worry about unloading
  ; the former texture/storing it outside the draw call.
  ; Can also consider a draw-text that also takes a top-left
  ; coordinate and directly calls the render-copy function.
  ; Maybe just do that and have implicit caching there.

  (cffi:with-foreign-object (c '(:struct sdl-color))
    (setf
      (cffi:foreign-slot-value c '(:struct sdl-color) 'r) r
      (cffi:foreign-slot-value c '(:struct sdl-color) 'g) g
      (cffi:foreign-slot-value c '(:struct sdl-color) 'b) b
      (cffi:foreign-slot-value c '(:struct sdl-color) 'a) a)
    (let* ((surf (lgame-sdl2-ttf.ffi:ttf-render-utf8-blended font text c))
           (tex (lgame::sdl-create-texture-from-surface lgame:*renderer* surf)))
      (lgame::sdl-free-surface surf)
      tex)))

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
