#|
From https://github.com/pygame/pygame/blob/main/examples/testsprite.py

Differences:
Not trying to duplicate original code too closely, just showing the same
feature -- a bunch of sprites bouncing around. On my machine, python
gets about 600 FPS at best. Lisp gets over 9000 FPS.

At 10,000 sprites, pygame gets 16 FPS, Lisp gets 240.
At 100,000 sprites, pygame gets 1.6 FPS, Lisp gets 20 FPS.
However, it took Lisp about 1 minute and 20 seconds to load up the sprites,
32 seconds if using an ordered-group, indicating inefficiencies in my group classes.

|#


;; quicklisp preamble and quickloading for script usage
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame))

(defpackage #:lgame.example.testsprite
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.sprite
                #:sprite
                #:.image
                #:.box
                #:update
                #:add-sprites
                #:group
                #:draw)
  (:import-from #:lgame.box
                #:move-box
                #:box-x
                #:box-y
                #:get-texture-box)
  (:import-from #:lgame.loader
                #:get-texture
                #:create-texture-loader)
  (:import-from #:lgame.event
                #:event-type))
(in-package #:lgame.example.testsprite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))

(defparameter +screen-size+ '(640 480))

(defparameter *total-sprites* 100)

(defun rand[] (a b)
  "Random int between a and b, inclusive"
  (+ a (random (1+ (- b a)))))

(defclass thingy (sprite)
  ((vel :accessor .vel :initform (list (rand[] -1 1) (rand[] -1 1)))))

(defmethod initialize-instance :after ((self thingy) &key)
  (setf (.image self) (get-texture "asprite.bmp" :color-key '(255 255 255))
        (.box self) (get-texture-box (.image self)))
  (move-box (.box self) (rand[] 0 (first +screen-size+)) (rand[] 0 (second +screen-size+))))

(defmethod update ((self thingy))
  (dotimes (i 2)
    (let* ((box (.box self))
           (nv (+ (if (zerop i) (box-x box) (box-y box)) (elt (.vel self) i))))
      (when (or (>= nv (elt +screen-size+ i)) (minusp nv))
        (setf (elt (.vel self) i) (- (elt (.vel self) i)))
        (setf nv (+ (if (zerop i) (box-x box) (box-y box)) (elt (.vel self) i))))
      (if (zerop i) (setf (box-x box) nv) (setf (box-y box) nv)))))


(defun main (&aux sprites frames)
  (lgame:init)
  (create-texture-loader *source-dir*)

  (lgame.display:create-centered-window "Testsprite" (first +screen-size+) (second +screen-size+))
  (lgame.display:create-renderer)

  (setf sprites (make-instance 'group))
  (dotimes (i *total-sprites*)
    (add-sprites sprites (make-instance 'thingy)))

  (setf frames 0)
  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (lgame.event:do-event (event)
            (when (find (event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+))
              (lgame.time:clock-stop)))
          (incf frames)
          (lgame.render:clear)
          (update sprites)
          (draw sprites)
          (lgame.render:present))

    (progn
      (format t "FPS: ~a~%" (/ frames (/ (lgame.time:clock-time) 1000.0)))
      (lgame:quit))))

(eval-when (:execute)
  (main))
