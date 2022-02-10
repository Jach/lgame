#|
Mostly equivalent to the example in the readme at https://github.com/borodust/trivial-gamekit
repeated here:
(gamekit:defgame example () ())

(defmethod gamekit:draw ((this example))
  (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 240.0 240.0)))

(gamekit:start 'example)

Lacks use of livesupport to not take over the REPL loop, doesn't provide an
abstraction like an lgame.sprite:sprite to change the rendered text while
running. As you can see, lgame is quite a bit more verbose.

|#

(ql:quickload :lgame)
(defpackage #:lgame.example.gamekit-hello-comparison
  (:use #:cl)
  (:export #:main))
(in-package #:lgame.example.gamekit-hello-comparison)

(defparameter *running?* t)

(defun main ()
  (lgame:init)
  (lgame.display:create-centered-window "Hello Comparison" 800 600)
  (lgame.display:create-renderer)

  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 15))
         (txt (lgame.font:render-text font "Hello, gamekit!" 0 0 0))
         (txt-rect (lgame.rect:get-texture-rect txt)))
    (lgame.rect:move-rect txt-rect 240 (- 600 240 (sdl2:rect-height txt-rect))) ; gamekit's origin is bottom-left, we are top-left following SDL
    (setf *running?* t)
    (lgame.time:clock-start)
    (unwind-protect
      (loop while *running?* do
            (game-tick txt txt-rect))

      (sdl2:free-rect txt-rect)
      (sdl2:destroy-texture txt)
      (lgame:quit))))

(defun game-tick (txt txt-rect)
  (lgame.event:do-event (event)
    (when (= (lgame.event:event-type event) lgame::+sdl-quit+)
      (setf *running?* nil)))

  (lgame.render:set-draw-color 255 255 255)
  (lgame.render:clear)

  (lgame.render:blit txt txt-rect)

  (lgame.render:present)

  (lgame.time:clock-tick 60))

(eval-when (:execute)
  (main))
