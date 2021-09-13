#|
From https://github.com/pygame/pygame/blob/main/examples/chimp.py
with explanation https://www.pygame.org/docs/tut/ChimpLineByLine.html

Differences:
Since this is a "larger" example, and can be used to show interactive
development of Lisp, the main game loop body is factored to its own
function, along with using https://github.com/cbaggers/livesupport
So if you were developing piecemeal, once you get the game loop
and screen going, you can just recompile it/modify passed data.
A bit more refactoring to have state bound outside the function
calls may be desired (e.g. make *allsprites* then you could dynamically
add to it, or the banner text so you could dynamically find the right
font/size/color/placement you prefer).

Our base Sprite class, besides image and rect slots, also contains slots
to handle flipping and rotation, so we don't need the same methods as the
original to accomplish that.

|#


(defpackage #:chimp
  (:use :cl))
(in-package #:chimp)

(defparameter *main-dir* (directory-namestring *load-truename*))
(defparameter *running?* t)

(push (merge-pathnames "../lgame/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload :lgame)
(ql:quickload :livesupport)

(defun load-sound (name)
  (sdl2-mixer:load-wav (merge-pathnames name *main-dir*)))

(defclass fist (lgame.sprite:sprite)
  ((punching? :accessor .punching? :initform nil))
  (:documentation "Moves a clenched fist on the screen, following the mouse"))

(defmethod initialize-instance :after ((self fist) &key)
  (let ((image (lgame:get-texture lgame:*texture-loader* :fist :color-key '(0 0 0))))
    (setf (lgame.sprite:image-of self) image)
    (setf (lgame.sprite:rect-of self) (lgame.rect:get-texture-rect image))))

(defmethod lgame.sprite:update ((self fist))
  "Move the fist based on mouse position"
  (with-accessors ((rect lgame.sprite:rect-of) (punching? .punching?)) self
    (multiple-value-bind (x y) (sdl2:mouse-state)
      (lgame.rect:set-rect rect :x x :y y)
      (lgame.rect:move-rect rect (/ (sdl2:rect-width rect) -2) 0))
    (when punching?
      (lgame.rect:move-rect rect 5 10))))

(defmethod punch ((self fist) target)
  "Set punch state, returns true if the fist collides with the target"
  (unless (.punching? self)
    (setf (.punching? self) t)
    (lgame.rect:with-inflated-rect (hitbox (lgame.sprite:rect-of self) -5 -5)
      (lgame.rect:collide-rect? (lgame.sprite:rect-of target) hitbox))))

(defmethod unpunch ((self fist))
  "Pull the fist back"
  (setf (.punching? self) nil))


(defclass chimp (lgame.sprite:sprite)
  ((move :accessor .move :initform 9))
  (:documentation "Moves a monkey across the screen, it can spin the monkey when it is punched"))

(defmethod initialize-instance :after ((self chimp) &key)
  (let ((image (lgame:get-texture lgame:*texture-loader* :chimp :color-key '(255 0 0))))
    (setf (lgame.sprite:image-of self) image)
    (setf (lgame.sprite:rect-of self) (lgame.rect:get-texture-rect image))
    (lgame.rect:move-rect (lgame.sprite:rect-of self) 10 10)))

(defmethod lgame.sprite:update ((self chimp))
  (if (zerop (lgame.sprite:angle-of self))
      (walk self)
      (spin self)))

(defmethod walk ((self chimp))
  "Moves monkey back and forth across the screen"
  (with-accessors ((rect lgame.sprite:rect-of) (speed .move) (flip lgame.sprite:flip-of)) self
    (lgame.rect:move-rect rect speed 0)
    (when (or (<= (lgame.rect:rect-dim rect :left) 0) (>= (lgame.rect:rect-dim rect :right) (lgame.rect:rect-dim lgame:*screen-rect* :right)))
      (setf speed (- speed))
      (lgame.rect:move-rect rect speed 0)
      (if (= flip lgame::+sdl-flip-none+)
          (setf flip lgame::+sdl-flip-horizontal+)
          (setf flip lgame::+sdl-flip-none+)))))

(defmethod spin ((self chimp))
  "Spin monkey image"
  (incf (lgame.sprite:angle-of self) 12)
  (if (>= (lgame.sprite:angle-of self) 360) ; complete rotation
      (setf (lgame.sprite:angle-of self) 0.0d0)))

(defmethod punched ((self chimp))
  "Cause the monkey to start spinning"
  (if (zerop (lgame.sprite:angle-of self))
      (setf (lgame.sprite:angle-of self) 1)))

(defun main ()
  (lgame:init)
  (lgame:create-texture-loader *main-dir*)
  (lgame.display:create-window "Monkey Fever" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ 468 60)
  (lgame.display:create-renderer)
  (sdl2:hide-cursor)

  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 20))
         (banner-txt (lgame.font:render-text font "Pummel The Chimp, And Win $$$" 10 10 10))
         (banner-txt-rect (lgame.rect:get-texture-rect banner-txt))

         (whiff-sound (load-sound "whiff.wav"))
         (punch-sound (load-sound "punch.wav"))
         (chimp (make-instance 'chimp))
         (fist (make-instance 'fist))
         (allsprites (make-instance 'lgame.sprite:group :sprites (list fist chimp))))

    (lgame.rect:move-rect banner-txt-rect (- (/ 468 2) (/ (sdl2:rect-width banner-txt-rect) 2)) 1) ; no centerx/centery construction yet

    (lgame.time:clock-start)
    (unwind-protect
      (loop while *running?* do
            (livesupport:continuable
              (game-tick banner-txt banner-txt-rect
                         whiff-sound punch-sound
                         chimp fist allsprites)))

      ; cleanup
      (lgame.sprite:cleanup allsprites)
      (sdl2:free-rect banner-txt-rect)
      (sdl2:destroy-texture banner-txt)
      (lgame.font:unload-fonts)
      (sdl2-mixer:free-chunk whiff-sound)
      (sdl2-mixer:free-chunk punch-sound)
      (lgame:quit))))

(defun game-tick (banner-txt banner-txt-rect whiff-sound punch-sound chimp fist allsprites)
  (lgame.event:do-event (event)
    (if (or
          (= (lgame.event:event-type event) lgame::+sdl-quit+)
          (and (= (lgame.event:event-type event) lgame::+sdl-keydown+)
               (= (lgame.event:key-scancode event) lgame::+sdl-scancode-escape+)))
        (setf *running?* nil))
    (if (= (lgame.event:event-type event) lgame::+sdl-mousebuttondown+)
        (if (punch fist chimp)
            (progn (sdl2-mixer:play-channel -1 punch-sound 0)
                   (punched chimp))
            (sdl2-mixer:play-channel -1 whiff-sound 0)))
    (if (= (lgame.event:event-type event) lgame::+sdl-mousebuttonup+)
        (unpunch fist)))

  (lgame.sprite:update allsprites)

  (lgame::sdl-set-render-draw-color lgame:*renderer* 170 238 187 255)
  (lgame::sdl-render-clear lgame:*renderer*)

  (sdl2:render-copy lgame:*renderer* banner-txt :dest-rect banner-txt-rect)

  (lgame.sprite:draw allsprites)

  (sdl2:render-present lgame:*renderer*)

  (lgame.time:clock-tick 60)

  (livesupport:update-repl-link))

(eval-when (:execute)
  (main))
