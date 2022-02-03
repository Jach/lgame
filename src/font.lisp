(in-package #:lgame.font)

(annot:enable-annot-syntax)

(defvar *loaded-fonts* (make-hash-table :test #'equal))

@export
(defun load-font (font-path pt-size)
  (multiple-value-bind (font present?) (gethash (namestring font-path) *loaded-fonts*)
    (unless present?
      (setf font (sdl2-ttf::ttf-open-font (namestring font-path) pt-size))
      (if (lgame:null-ptr? font)
          (error 'sdl-error :msg (lgame::sdl-get-error))) ; add restart to use default font
      (setf (gethash (namestring font-path) *loaded-fonts*) font))
    font))

@export
(defun unload-fonts ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (lgame::ttf-close-font v))
           *loaded-fonts*)
  (clrhash *loaded-fonts*))

(defun no-finalized-render-text-solid (font text r g b a)
  "Can't just call the underlying ttf-render-text-solid
   because call-by-value was not added by sdl2-ttf.
   However I don't yet want to call sdl2-ttf:render-text-solid
   because it puts the surface in a finalizer which may eventually
   be freed by the GC, I'd like to just free it immediately."
  (let ((surf (sdl2-ffi::make-sdl-surface
                :ptr
                (sdl2-ttf::%sdl-render-text-solid (sdl2-ttf::ptr font)
                                                  text
                                                  (sdl2-ttf::create-sdl-color-list r g b a)))))
    (if (lgame:null-ptr? surf)
        (error 'sdl-error :msg (lgame::sdl-get-error))
        surf)))

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
  (sdl2-ttf:render-text-solid font text r g b a)
  (let* ((surface (no-finalized-render-text-solid font text r g b a))
         (texture (sdl2:create-texture-from-surface lgame:*renderer* surface)))
    (sdl2:free-surface surface)
    texture))

@export
(defun get-default-font ()
  (asdf:system-relative-pathname :lgame "assets/open-sans/OpenSans-Regular.ttf"))

