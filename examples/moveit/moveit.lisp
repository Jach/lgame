#|
From https://github.com/pygame/pygame/blob/main/examples/moveit.py
along with tutorial https://www.pygame.org/docs/tut/MoveIt.html

Notable differences: we don't need to create a 4x sized background surface for
the background to make it fill up the screen, we can just render directly
without specifying a dst-rect and it will scale for us basically for free.

Also the classic "dirty rects" optimization doesn't really work the same
anymore and isn't even really needed, so we just draw the whole background each
frame. It can be done, but not on the final rendered screen, you'd need to do
it on your own streaming/target texture and render that texture every frame.
|#

(defpackage #:moveit
  (:use :cl))
(in-package #:moveit)

(defparameter *main-name* *load-truename*)
(defparameter *running?* t)

(ql:quickload :lgame)

(defclass game-object ()
  ((image :initarg :image :reader .image)
   (speed :initarg :speed :reader .speed)
   (pos :reader .pos)))

(defmethod initialize-instance :after ((self game-object) &key (height 0) &allow-other-keys)
  (setf (slot-value self 'pos) (lgame.rect:get-texture-rect (.image self)))
  (lgame.rect:move-rect (.pos self) 0 height))

(defmethod move ((self game-object))
  (lgame.rect:move-rect (.pos self) (.speed self) 0)
  (when (> (lgame.rect:rect-dim (.pos self) :right) 600)
    (lgame.rect:set-rect (.pos self) :x 0)))

(defun load-image (name)
  (lgame.loader:load-texture (merge-pathnames name (directory-namestring *main-name*))))

(defun main (&aux player background objects)
  (lgame:init)
  (lgame.display:create-window "moveit" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ 640 480)
  (lgame.display:create-renderer)

  (setf player (load-image "player1.png"))
  (setf background (load-image "liquid.png"))

  (setf objects (loop for x below 10 collect
                      (make-instance 'game-object :image player :speed x :height (* x 40))))

  (setf *running?* t)
  (loop while *running?* do
    (sleep (/ 1 60.0)) ; even the original is super fast, this is a simple way to slow things down, accurate way in a future example
    (lgame.event:do-event (event)
      (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+) :test #'=)
          (setf *running?* nil)))

    (sdl2:render-copy lgame:*renderer* background)

    (dolist (o objects)
      (move o)
      (sdl2:render-copy lgame:*renderer* (.image o) :dest-rect (.pos o)))

    (sdl2:render-present lgame:*renderer*))
  (lgame:quit))

(eval-when (:execute)
  (main))
