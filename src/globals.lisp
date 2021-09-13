(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defvar *dt* 0.0)

(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))

