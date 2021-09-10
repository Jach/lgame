(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defvar *dt* 0.0)

@export
(defvar *screen*)

@export
(defvar *screen-rect*)

@export
(defvar *renderer*)

@export
(defvar *texture-loader*)

(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))

