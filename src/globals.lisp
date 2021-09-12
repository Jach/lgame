(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defvar *dt* 0.0)

@export
(defvar *screen* nil)

@export
(defvar *screen-rect* nil)

@export
(defvar *renderer* nil)

@export
(defvar *texture-loader* nil)

(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))

