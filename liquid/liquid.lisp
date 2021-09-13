#|
Based on https://github.com/pygame/pygame/blob/main/examples/liquid.py
Renders a neat bouncy/liquidy effect.

Notable differences:

Instead of loading liquid.png and 4x'ing it, we instead create a texture that's
4x its size, and render the image to it, getting nice scaling for free. You can
uncomment the line about adjusting the render-scale-quality to change the
scaling from the default "nearest" (nearest pixel sampling) to "linear" (linear
filtering) and see its effects: slightly blurrier/softer edges, I don't think it
looks as good, it seems to make it easier to detect the 20x20 cells due to
aliasing, and the 'jaggies' of the underlying image are still very visible.
Pygame's transform.scale2x() wins here because it's using AdvanceMAME Scale2x
which does a 'jaggie-less' scaling. Maybe I'll implement the algorithm in lgame
too at a later time.

Why not use the window size, in the original or this one? You can, but you'll
see interesting behavior at the edges, because it will be trying to pull data
from outside the texture. For the effect to work, the maximum perturbation of
the data should still be within the texture.

|#
(defpackage #:liquid
  (:use :cl))
(in-package #:liquid)

(defparameter *main-dir* (directory-namestring *load-truename*))
(defparameter *running?* t)

(push (merge-pathnames "../lgame/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload :lgame)
(ql:quickload :cmu-infix)
(named-readtables:in-readtable cmu-infix:syntax)

(defun main (&aux background anim)
  (lgame:init)
  (lgame.display:create-window "liquid" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ 640 480)
  (lgame.display:create-renderer)

  ;(lgame::sdl-set-hint lgame::+sdl-hint-render-scale-quality+ "linear")
  (let ((liquid (lgame.loader:load-texture (merge-pathnames "../moveit/liquid.png" *main-dir*))))
    (setf background (sdl2:create-texture lgame:*renderer* lgame::+sdl-pixelformat-rgba8888+ lgame::+sdl-textureaccess-target+ (* (sdl2:texture-width liquid) 4) (* (sdl2:texture-height liquid) 4)))
    (sdl2:set-render-target lgame:*renderer* background)
    (sdl2:render-copy lgame:*renderer* liquid)
    (sdl2:set-render-target lgame:*renderer* nil)
    (sdl2:destroy-texture liquid))

  (setf anim 0.0)

  (setf *running?* t)
  (lgame.time:clock-start)
  (loop while *running?* do
    (lgame.event:do-event (event)
      (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+ ,lgame::+sdl-mousebuttondown+))
          (setf *running?* nil)))

    (incf anim 0.2)
    (loop for x from 0 below 640 by 20 do
          (let ((xpos #I(x + sin(anim + x*0.01)*15 + 20)))
            (loop for y from 0 below 480 by 20 do
                  (uiop:nest
                    (let ((ypos #I(y + sin(anim + y*0.01)*15 + 20))))
                    (lgame.rect:with-rect (source (round xpos) (round ypos) 20 20))
                    (lgame.rect:with-rect (dest x y 20 20))
                    (sdl2:render-copy lgame:*renderer* background :source-rect source :dest-rect dest)))))

    (sdl2:render-present lgame:*renderer*)
    (lgame.time:clock-tick 60))

  (sdl2:destroy-texture background)
  (lgame:quit))

(eval-when (:execute)
  (main))
