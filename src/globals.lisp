(in-package #:lgame)

(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))
