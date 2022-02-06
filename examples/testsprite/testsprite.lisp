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


(ql:quickload :lgame)

(defpackage #:testsprite
  (:use :cl))
(in-package #:testsprite)

(defparameter +screen-size+ '(640 480))
(defparameter *main-dir* (directory-namestring *load-truename*))
(defparameter *running?* t)

(defun rand[] (a b)
  "Random int between a and b, inclusive"
  (+ a (random (1+ (- b a)))))

(defclass thingy (lgame.sprite:sprite)
  ((vel :accessor .vel :initform (list (rand[] -1 1) (rand[] -1 1)))))

(defmethod initialize-instance :after ((self thingy) &key)
  (setf (lgame.sprite:.image self) (lgame.loader:get-texture "asprite.bmp" :color-key '(255 255 255))
        (lgame.sprite:.rect self) (lgame.rect:get-texture-rect (lgame.sprite:.image self)))
  (lgame.rect:move-rect (lgame.sprite:.rect self) (rand[] 0 (first +screen-size+)) (rand[] 0 (second +screen-size+))))

(defmethod lgame.sprite:update ((self thingy))
  (dotimes (i 2)
    (let* ((rect (lgame.sprite:.rect self))
           (nv (+ (if (zerop i) (sdl2:rect-x rect) (sdl2:rect-y rect)) (elt (.vel self) i))))
      (when (or (>= nv (elt +screen-size+ i)) (minusp nv))
        (setf (elt (.vel self) i) (- (elt (.vel self) i)))
        (setf nv (+ (if (zerop i) (sdl2:rect-x rect) (sdl2:rect-y rect)) (elt (.vel self) i))))
      (if (zerop i) (setf (sdl2:rect-x rect) nv) (setf (sdl2:rect-y rect) nv)))))


(defun main (&aux sprites frames)
  (lgame:init)
  (lgame.loader:create-texture-loader *main-dir*)
  (lgame.display:create-window "Testsprite" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+
                               (first +screen-size+) (second +screen-size+))
  (lgame.display:create-renderer)

  (setf sprites (make-instance 'lgame.sprite:group))
  (dotimes (i 100)
    (lgame.sprite:add-sprites sprites (make-instance 'thingy)))

  (setf frames 0)
  (lgame.time:clock-start)
  (setf *running?* t)
  (loop while *running?* do
        (lgame.event:do-event (event)
          (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+))
              (setf *running?* nil)))
        (incf frames)
        (sdl2:render-clear lgame:*renderer*)
        (lgame.sprite:update sprites)
        (lgame.sprite:draw sprites)
        (sdl2:render-present lgame:*renderer*))
  (format t "FPS: ~a~%" (/ frames (/ (lgame.time:clock-time) 1000.0)))
  (lgame:quit))

(eval-when (:execute)
  (main))
