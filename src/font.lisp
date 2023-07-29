(in-package #:lgame.font)

(annot:enable-annot-syntax)

(defvar *loaded-fonts* (make-hash-table :test #'equal))

@export
(defun load-font (font-path pt-size)
  (multiple-value-bind (font present?) (gethash (namestring font-path) *loaded-fonts*)
    (unless present?
      (setf font (lgame-sdl2-ttf.ffi:ttf-open-font (namestring font-path) pt-size))
      (if (lgame:null-ptr? font)
          (error 'sdl-error :msg (lgame::sdl-get-error))) ; add restart to use default font
      (setf (gethash (namestring font-path) *loaded-fonts*) font))
    font))

@export
(defun unload-fonts ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (lgame-sdl2-ttf.ffi:ttf-close-font v))
           *loaded-fonts*)
  (clrhash *loaded-fonts*))

(cffi:defcstruct sdl-color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

@export
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
    (let* ((surf (lgame-sdl2-ttf.ffi:ttf-render-utf8-solid font text c))
           (tex (lgame::sdl-create-texture-from-surface lgame:*renderer* surf)))
      (lgame::sdl-free-surface surf)
      tex)))

@export
(defun get-default-font ()
  (asdf:system-relative-pathname :lgame "assets/open-sans/OpenSans-Regular.ttf"))

