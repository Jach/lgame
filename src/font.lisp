(in-package #:lgame)

(annot:enable-annot-syntax)

(defvar *loaded-fonts* (make-hash-table :test #'equal))

@export
(defun load-font (font-path pt-size)
  (multiple-value-bind (font present?) (gethash (namestring font-path) *loaded-fonts*)
    ; check null-ptr
    (unless present?
      (setf font (ttf-open-font (namestring font-path) pt-size))
      (setf (gethash (namestring font-path) *loaded-fonts*) font))
    font))

@export
(defun unload-fonts ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (ttf-close-font v))
           *loaded-fonts*)
  (clrhash *loaded-fonts*))

