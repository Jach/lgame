#|
From https://github.com/pygame/pygame/blob/main/examples/aliens.py

Controls:
Left/right to move
Space to shoot
f key to toggle between fullscreen

Differences:
* Somewhat different alien odds
* We don't need separate images to handle simple flips
* Groups are handled more explicitly, by passing in a :groups in the constructor rather than
  setting a class-allocated container field that is automatically used by the constructor
  (transparently by the add-groups-mixin). Also kills are more explicit.
* Since we're using lgame:*texture-loader* which caches textures, there's not a penalty if we get the sprite image
  each time in the constructor. We still class-allocate frames of images, though, so the list of frames is at least only stored once.
* No dirty rects
* Sane full-screen that doesn't mess with the user's monitor settings,
  instead just scaling to whatever their desktop resolution is and having
  cheap gpu scaling while retaining the logical render size.
* Short delay before the attack starts
* Window focusing when program starts

|#

(push (merge-pathnames "../lgame/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload :lgame)

(defpackage #:aliens
  (:use :cl)
  (:import-from #:lgame.sprite
                #:sprite
                #:cleaned-on-kill-mixin
                #:add-groups-mixin
                #:.image
                #:.rect
                #:.flip

                #:update
                #:draw
                #:kill)
  (:import-from #:lgame.rect
                #:rect-dim))
(in-package #:aliens)

(defparameter *main-dir* (directory-namestring *load-truename*))
(defparameter *running?* t)

(defparameter +max-shots+ 2) ; player shots onscreen
(defparameter +alien-odds+ 32) ; odds new alien appears
(defparameter +bomb-odds+ 60) ; odds new bomb will drop from alien
(defparameter +alien-reload+ 12) ; min frames between new aliens
(defparameter *current-alien-reload* 12)

(defparameter +screen-size+ '(640 480))
(defparameter *full-screen* nil)

(defclass player (sprite add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform 10)
   (bounce :accessor /bounce :allocation :class :initform 24)
   (gun-offset :accessor /gun-offset :allocation :class :initform -11)

   (reloading? :accessor .reloading?
               :initform nil)
   (origtop :accessor .origtop)))

(defmethod initialize-instance :after ((self player) &key)
  (setf (.image self) (lgame.loader:get-texture lgame:*texture-loader* "player1.png")
        (.rect self) (lgame.rect:get-texture-rect (.image self))
        (rect-dim (.rect self) :midbottom) (rect-dim lgame:*screen-rect* :midbottom)

        (.origtop self) (rect-dim (.rect self) :top)))

(defmethod move ((self player) direction)
  (declare (type (member :left :right) direction))
  (case direction
    (:left
      (setf (.flip self) lgame::+sdl-flip-none+)
      (lgame.rect:move-rect (.rect self) (- (/speed self)) 0))
    (:right
      (setf (.flip self) lgame::+sdl-flip-horizontal+)
      (lgame.rect:move-rect (.rect self) (/speed self) 0)))

  ; forbid moving outside the screen:
  (lgame.rect:clamp (.rect self) lgame:*screen-rect*)
  ; bouncing effect:
  (setf (rect-dim (.rect self) :top) (- (.origtop self) (mod (truncate (/ (rect-dim (.rect self) :left)
                                                                          (/bounce self)))
                                                             2))))

(defmethod gunpos ((self player))
  (list (+ (if (eql (.flip self) lgame::+sdl-flip-none+)
               (- (/gun-offset self))
               (/gun-offset self))
           (rect-dim (.rect self) :centerx))
        (rect-dim (.rect self) :top)))


(defclass alien (sprite add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform 13)
   (animcycle :accessor /animcycle :allocation :class :initform 12)
   (image-frames :accessor /image-frames :allocation :class :initform (list))

   (facing :accessor .facing)
   (frame :accessor .frame)))

(defun random-choice (choices)
  (elt choices (random (length choices))))

(defmethod initialize-instance :after ((self alien) &key)
  (unless (/image-frames self)
    (setf (/image-frames self) (list (lgame.loader:get-texture lgame:*texture-loader* "alien1.png")
                                     (lgame.loader:get-texture lgame:*texture-loader* "alien2.png")
                                     (lgame.loader:get-texture lgame:*texture-loader* "alien3.png"))))
  (setf (.image self) (first (/image-frames self))
        (.rect self) (lgame.rect:get-texture-rect (.image self))
        (.facing self) (* (/speed self) (random-choice '(-1 1)))
        (.frame self) 0)
  (when (minusp (.facing self))
    (setf (rect-dim (.rect self) :right) (rect-dim lgame:*screen-rect* :right))))

(defmethod update ((self alien))
  (with-accessors ((rect .rect) (facing .facing) (frame .frame) (image .image)) self
    (lgame.rect:move-rect rect (.facing self) 0)
    (unless (lgame.rect:contains? lgame:*screen-rect* rect) ; alien reached the edge of screen
      (setf facing (- facing)
            (rect-dim rect :top) (1+ (rect-dim rect :bottom)))
      (lgame.rect:clamp rect lgame:*screen-rect*))
    (incf frame) ; change frame image every animcycle frames
    (setf image (elt (/image-frames self) (mod (truncate (/ frame (/animcycle self))) (length (/image-frames self)))))))


(defclass explosion (sprite cleaned-on-kill-mixin add-groups-mixin)
  ((lifetime :accessor /lifetime :allocation :class :initform 12)
   (animcycle :accessor /animcycle :allocation :class :initform 3)

   (life :accessor .life))
  (:documentation "Explosion for both alien and player,
                   lasts by default 12 frames, 'animating' between
                   a flipped image/not every 3 frames."))

(defmethod initialize-instance :after ((self explosion) &key actor)
  (setf (.image self) (lgame.loader:get-texture lgame:*texture-loader* "explosion1.png")
        (.rect self) (lgame.rect:get-texture-rect (.image self))
        (.life self) (/lifetime self))
  (if actor
  (setf (rect-dim (.rect self) :center) (rect-dim (.rect actor) :center))))

(defmethod update ((self explosion))
  (decf (.life self))
  (if (zerop (mod (truncate (/ (.life self) (/animcycle self))) 2))
      (setf (.flip self) lgame::+sdl-flip-none+)
      (setf (.flip self) lgame::+sdl-flip-horizontal+))
  (unless (plusp (.life self))
    (kill self)))


(defclass shot (sprite cleaned-on-kill-mixin add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform -11)))

(defmethod initialize-instance :after ((self shot) &key pos)
  (setf (.image self) (lgame.loader:get-texture lgame:*texture-loader* "shot.png")
        (.rect self) (lgame.rect:get-texture-rect (.image self))
        (rect-dim (.rect self) :midbottom) pos))

(defmethod update ((self shot))
  (lgame.rect:move-rect (.rect self) 0 (/speed self))
  (unless (plusp (rect-dim (.rect self) :top))
    (kill self)))


(defclass bomb (sprite cleaned-on-kill-mixin add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform 9)))

(defmethod initialize-instance :after ((self bomb) &key alien)
  (setf (.image self) (lgame.loader:get-texture lgame:*texture-loader* "bomb.png")
        (.rect self) (lgame.rect:get-texture-rect (.image self)))
  (lgame.rect:with-moved-rect (moved (.rect alien) 0 5)
    (setf (rect-dim (.rect self) :midbottom) (rect-dim moved :midbottom))))

(defmethod update ((self bomb))
  (lgame.rect:move-rect (.rect self) 0 (/speed self))
  (when (>= (rect-dim (.rect self) :bottom) (- (rect-dim lgame:*screen-rect* :bottom) 10))
    (make-instance 'explosion :actor self :groups (lgame.sprite:.groups self))
    (kill self)))


(defclass score (sprite add-groups-mixin)
  ((score :accessor .score :initform 0)
   (last-score :accessor .last-score :initform -1)))

(defmethod initialize-instance :after ((self score) &key)
  (setf (.image self) nil)
  (update self)
  (setf (.rect self) (lgame.rect:get-texture-rect (.image self)))
  (lgame.rect:move-rect (.rect self) 10 (- (rect-dim lgame:*screen-rect* :bottom) 30)))

(defmethod update ((self score))
  (when (/= (.score self) (.last-score self))
    (setf (.last-score self) (.score self))
    (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 20))
           (msg (format nil "Score: ~a" (.score self)))
           (new-score-texture (lgame.font:render-text font msg 255 255 255)))
      (when (.image self)
        (sdl2:destroy-texture (.image self)))
      (setf (.image self) new-score-texture))))


(defun main (&aux background boom-sound shoot-sound music groups group-lists player score)
  (lgame:init)
  (lgame.loader:create-texture-loader *main-dir*)
  (lgame.display:create-window "Aliens?!" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+
                               (first +screen-size+) (second +screen-size+))
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size (first +screen-size+) (second +screen-size+))
  (sdl2:hide-cursor)

  (sdl2:pump-events)
  (lgame::sdl-raise-window lgame:*screen*)

  (let ((bg-tile (lgame.loader:get-texture lgame:*texture-loader* "background.png")))
    (setf background (sdl2:create-texture lgame:*renderer* lgame::+sdl-pixelformat-rgba8888+ lgame::+sdl-textureaccess-target+
                                          (first +screen-size+) (second +screen-size+)))
    (sdl2:set-render-target lgame:*renderer* background)
    (loop for x from 0 below (first +screen-size+) by (sdl2:texture-width bg-tile) do
          (lgame.rect:with-rect (r x 0 (sdl2:texture-width bg-tile) (sdl2:texture-height bg-tile))
            (sdl2:render-copy lgame:*renderer* bg-tile :dest-rect r)))
    (sdl2:set-render-target lgame:*renderer* nil))

  (setf boom-sound (sdl2-mixer:load-wav (merge-pathnames "boom.wav" *main-dir*)))
  (setf shoot-sound (sdl2-mixer:load-wav (merge-pathnames "car_door.wav" *main-dir*)))
  (setf music (sdl2-mixer:load-music (merge-pathnames "house_lo.wav" *main-dir*)))
  (sdl2-mixer:play-music music -1)

  (setf groups (list :aliens (make-instance 'lgame.sprite:group)
                     :shots (make-instance 'lgame.sprite:group)
                     :bombs (make-instance 'lgame.sprite:group)
                     :all (make-instance 'lgame.sprite:ordered-group)
                     :last-alien (make-instance 'lgame.sprite:group-single)))

  (setf group-lists (list :for-player (list (getf groups :all))
                          :for-alien (list (getf groups :aliens) (getf groups :all) (getf groups :last-alien))
                          :for-shot (list (getf groups :shots) (getf groups :all))
                          :for-bomb (list (getf groups :bombs) (getf groups :all))
                          :for-explosion (list (getf groups :all))
                          :for-score (list (getf groups :all))))

  (setf player (make-instance 'player :groups (getf group-lists :for-player)))
  (setf score (make-instance 'score :groups (getf group-lists :for-score)))

  (setf *current-alien-reload* +alien-reload+)
  (lgame.time:clock-start)
  (setf *running?* t)
  (unwind-protect
    (loop while *running?* do
          (game-tick background boom-sound shoot-sound groups group-lists player score))


    (sdl2-mixer:halt-music)
    (sdl2:destroy-texture background)
    (lgame.sprite:cleanup (getf groups :all))
    ; may not need to be called because of sdl2-mixer's usage of autocollect...
    ;(sdl2-mixer:free-music music)
    ;(sdl2-mixer:free-chunk shoot-sound)
    ;(sdl2-mixer:free-chunk boom-sound)

    (lgame.font:unload-fonts)
    (lgame:quit)))

(defun game-tick (background boom-sound shoot-sound groups group-lists player score)
  (lgame.event:do-event (event)
    (if (or
          (= (lgame.event:event-type event) lgame::+sdl-quit+)
          (and (= (lgame.event:event-type event) lgame::+sdl-keydown+)
               (= (lgame.event:key-scancode event) lgame::+sdl-scancode-escape+)))
        (setf *running?* nil))
    (if (and (= (lgame.event:event-type event) lgame::+sdl-keydown+)
             (= (lgame.event:key-scancode event) lgame::+sdl-scancode-f+))
        (if *full-screen*
            (progn (lgame::sdl-set-window-fullscreen lgame:*screen* 0) (setf *full-screen* nil))
            (progn (lgame::sdl-set-window-fullscreen lgame:*screen* lgame::+sdl-window-fullscreen-desktop+) (setf *full-screen* t)))))

  (update (getf groups :all))

  (handle-player-input player groups group-lists shoot-sound)

  (when (>= (lgame::sdl-get-ticks) 3000)
    (spawn-alien group-lists))

  (spawn-bombs groups group-lists)

  (handle-alien-player-collision player groups group-lists score boom-sound)
  (handle-alien-shot-collisions groups group-lists score boom-sound)
  (handle-bombs-player-collisions player groups group-lists boom-sound)

  (sdl2:render-clear lgame:*renderer*)
  (sdl2:render-copy lgame:*renderer* background)
  (draw (getf groups :all))
  (sdl2:render-present lgame:*renderer*)

  (lgame.time:clock-tick 40)
  )

(defun handle-player-input (player groups group-lists shoot-sound)
  (if (lgame.event:key-pressed? :key lgame::+sdl-scancode-right+)
      (move player :right))
  (if (lgame.event:key-pressed? :key lgame::+sdl-scancode-left+)
      (move player :left))
  (let ((firing (lgame.event:key-pressed? :key lgame::+sdl-scancode-space+)))
    (when (and (not (.reloading? player))
               firing
               (< (length (lgame.sprite:.sprites (getf groups :shots))) +max-shots+))
      (make-instance 'shot :groups (getf group-lists :for-shot) :pos (gunpos player))
      (sdl2-mixer:play-channel -1 shoot-sound 0))
    (setf (.reloading? player) firing))) ; prevents holding down space key to fire (why not just use keydown/keyup events?)
                                          ; i.e. must see space key not-pressed before able to fire again

(defun spawn-alien (group-lists)
  (decf *current-alien-reload*)
  (when (and (not (plusp *current-alien-reload*))
             (zerop (random +alien-odds+)))
    (setf *current-alien-reload* +alien-reload+)
    (make-instance 'alien :groups (getf group-lists :for-alien))))

(defun spawn-bombs (groups group-lists)
  ; drop bombs (only last alien can do so)
  (when (and (not (lgame.sprite:empty? (getf groups :last-alien)))
             (zerop (random +bomb-odds+)))
    (make-instance 'bomb :groups (getf group-lists :for-bomb) :alien (lgame.sprite:.sprite (getf groups :last-alien)))))

(defun handle-alien-player-collision (player groups group-lists score boom-sound)
  (dolist (alien (lgame.sprite:sprite-collide player (getf groups :aliens)))
    (sdl2-mixer:play-channel -1 boom-sound 0)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor alien)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor player)
    (incf (.score score))
    (setf *running?* nil)
    (kill player)
    (kill alien)))

(defun handle-alien-shot-collisions (groups group-lists score boom-sound)
  (dolist (collisions (lgame.sprite:group-collide (getf groups :aliens) (getf groups :shots)))
    (let ((alien (car collisions))
          (shots (cdr collisions)))
      (sdl2-mixer:play-channel -1 boom-sound 0)
      (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor alien)
      (incf (.score score))
      (kill alien)
      (mapc #'kill shots))))

(defun handle-bombs-player-collisions (player groups group-lists boom-sound)
  (dolist (bomb (lgame.sprite:sprite-collide player (getf groups :bombs)))
    (sdl2-mixer:play-channel -1 boom-sound 0)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor player)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor bomb)
    (kill bomb)
    (kill player)
    (setf *running?* nil)))

(eval-when (:execute)
  (main))
