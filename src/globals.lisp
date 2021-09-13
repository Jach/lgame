(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))
