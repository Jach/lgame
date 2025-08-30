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

This example should free up its foreign memory (textures and rects) but doesn't.
|#

;; quicklisp preamble and quickloading for script usage
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame))

(defpackage #:lgame.example.moveit
  (:use #:cl)
  (:export #:main))
(in-package #:lgame.example.moveit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))

(defun load-image (name)
  (lgame.loader:load-texture (merge-pathnames name *source-dir*)))


(defclass game-object () ; lgame.sprite:sprite inheritance can make this a bit nicer
  ((image :initarg :image :reader .image) ; texture, passed in make-instance
   (speed :initarg :speed :reader .speed) ; horizontal speed
   (pos :reader .pos))) ; position but also bounding box of image

(defmethod initialize-instance :after ((self game-object) &key (height 0) &allow-other-keys)
  (setf (slot-value self 'pos) (lgame.box:get-texture-box (.image self)))
  (lgame.box:move-box (.pos self) 0 height))

(defmethod move ((self game-object))
  (lgame.box:move-box (.pos self) (.speed self) 0)
  (when (> (lgame.box:box-attr (.pos self) :right) 600) ; reset to other side of screen
    (lgame.box:set-box (.pos self) :x 0)))


(defun main (&aux player background objects)
  (lgame:init)
  (lgame.display:create-centered-window "moveit" 640 480)
  (lgame.display:create-renderer)

  (setf player (load-image "player1.png"))
  (setf background (load-image "liquid.png"))

  (setf objects (loop for x below 10 collect
                      (make-instance 'game-object :image player :speed x :height (* x 40))))

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (lgame.event:do-event (event)
            (when (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+) :test #'=)
              (lgame.time:clock-stop)))

          (lgame.render:blit background nil)

          (dolist (o objects)
            (move o)
            (lgame.render:blit (.image o) (.pos o)))

          (lgame.render:present)

          (lgame.time:clock-tick 60))

    (lgame:quit)))

(eval-when (:execute)
  (main))
