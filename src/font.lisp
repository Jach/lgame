(in-package #:lgame)

(annot:enable-annot-syntax)

(defvar *loaded-fonts* (make-hash-table :test #'equal))

@export
(defun load-font (font-path pt-size)
  (multiple-value-bind (font present?) (gethash (namestring font-path) *loaded-fonts*)
    ; check null-ptr
    (unless present?
      (setf font (sdl2-ttf:open-font (namestring font-path) pt-size))
      (setf (gethash (namestring font-path) *loaded-fonts*) font))
    font))

@export
(defun unload-fonts ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (ttf-close-font v))
           *loaded-fonts*)
  (clrhash *loaded-fonts*))

@export
(defun render-text (font text r g b &optional (a 255))
  (let* ((surface (sdl2-ttf:render-text-solid font text r g b a))
         (texture (sdl-create-texture-from-surface lgame:*renderer* surface))) ; check errors..
    (sdl-free-surface surface)
    texture))

@export
(defun get-default-font ()
  (asdf:system-relative-pathname :lgame "assets/open-sans/OpenSans-Regular.ttf"))
