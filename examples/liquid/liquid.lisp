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

;; quicklisp preamble and quickloading for script usage
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame)
  (ql:quickload :cmu-infix))

(defpackage #:lgame.example.liquid
  (:use #:cl)
  (:export #:main))
(in-package #:lgame.example.liquid)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))

(named-readtables:in-readtable cmu-infix:syntax)

(defun main (&aux background anim)
  (lgame:init)
  (lgame.display:create-centered-window "liquid" 640 480)
  (lgame.display:create-renderer)

  ;(lgame::sdl-set-hint lgame::+sdl-hint-render-scale-quality+ "linear")
  (let ((liquid (lgame.loader:load-texture (merge-pathnames "../moveit/liquid.png" *source-dir*))))
    (setf background (lgame.texture:create-empty-sdl-texture lgame:*renderer*
                                                             lgame::+sdl-textureaccess-target+
                                                             (* (lgame.texture:.width liquid) 4)
                                                             (* (lgame.texture:.height liquid) 4)))
    (lgame.render:with-render-target background
      (lgame.render:blit liquid nil))
    (lgame.texture:destroy-texture liquid))

  (setf anim 0.0)

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (lgame.event:do-event (event)
            (when (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+ ,lgame::+sdl-mousebuttondown+))
              (lgame.time:clock-stop)))

          (incf anim 0.2)
          (loop for x from 0 below 640 by 20 do
                (let ((xpos #I(x + sin(anim + x*0.01)*15 + 20)))
                  (loop for y from 0 below 480 by 20 do
                        (let* ((ypos #I(y + sin(anim + y*0.01)*15 + 20))
                               (source (lgame.box:make-box xpos ypos 20 20))
                               (dest (lgame.box:make-box x y 20 20)))
                          (lgame.render:blit background dest source)))))

          (lgame.render:present)

          (lgame.time:clock-tick 60))

    (progn
      (lgame.texture:destroy-texture background)
      (lgame:quit))))

(eval-when (:execute)
  (main))
