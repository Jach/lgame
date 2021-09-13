(in-package #:lgame)

(annot:enable-annot-syntax)

(defvar *loaded-fonts* (make-hash-table :test #'equal))

@export
(defun load-font (font-path pt-size)
  (multiple-value-bind (font present?) (gethash (namestring font-path) *loaded-fonts*)
    (unless present?
      (setf font (sdl2-ttf::ttf-open-font (namestring font-path) pt-size))
      (if (null-ptr? font)
          (error 'sdl-error :msg (sdl-get-error))) ; add restart to use default font
      (setf (gethash (namestring font-path) *loaded-fonts*) font))
    font))

@export
(defun unload-fonts ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (ttf-close-font v))
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
    (if (null-ptr? surf)
        (error 'sdl-error :msg (sdl-get-error))
        surf)))

@export
(defun render-text (font text r g b &optional (a 255))
  (sdl2-ttf:render-text-solid font text r g b a)
  (let* ((surface (no-finalized-render-text-solid font text r g b a))
         (texture (sdl2:create-texture-from-surface lgame:*renderer* surface)))
    (sdl2:free-surface surface)
    texture))

@export
(defun get-default-font ()
  (asdf:system-relative-pathname :lgame "assets/open-sans/OpenSans-Regular.ttf"))
