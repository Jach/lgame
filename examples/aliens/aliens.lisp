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
* Since we're using lgame.loader functions which caches textures, there's not a penalty if we get the sprite image
  each time in the constructor. We still class-allocate frames of images, though, so the list of frames is at least only stored once.
* No dirty rects
* Sane full-screen that doesn't mess with the user's monitor settings,
  instead just scaling to whatever their desktop resolution is and having
  cheap gpu scaling while retaining the logical render size.
* Short delay before the attack starts
* Window focusing when program starts

|#

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame)
  (ql:quickload :closer-mop))

(defpackage #:lgame.example.aliens
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.sprite
                #:sprite
                #:add-groups-mixin
                #:.image
                #:.box
                #:.flip

                #:update
                #:draw
                #:kill)
  (:import-from #:lgame.box
                #:box-attr
                #:move-box
                #:with-moved-box
                #:get-texture-box
                #:clamp
                #:box-contains?
                #:make-box
                #:with-box-as-sdl-rect)

  (:import-from #:lgame.event
                #:event-type
                #:key-scancode
                #:do-event))

(in-package #:lgame.example.aliens)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))
(defparameter *running?* t)

(defparameter +max-shots+ 2) ; player shots onscreen
(defparameter +alien-odds+ 32) ; odds new alien appears
(defparameter +bomb-odds+ 60) ; odds new bomb will drop from alien
(defparameter +alien-reload+ 12) ; min frames between new aliens
(defparameter *current-alien-reload* 12) ; how many frames left until there's a chance of the next alien

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
  (setf (.image self) (lgame.loader:get-texture "player1.png")
        (.box self) (get-texture-box (.image self))
        (box-attr (.box self) :midbottom) (box-attr lgame:*screen-box* :midbottom)

        (.origtop self) (box-attr (.box self) :top)))

(defmethod move ((self player) direction)
  (declare (type (member :left :right) direction))
  (case direction
    (:left
      (setf (.flip self) lgame::+sdl-flip-none+)
      (move-box (.box self) (- (/speed self)) 0))
    (:right
      (setf (.flip self) lgame::+sdl-flip-horizontal+)
      (move-box (.box self) (/speed self) 0)))

  ; forbid moving outside the screen:
  (lgame.box:clamp (.box self) lgame:*screen-box*)
  ; bouncing effect:
  (setf (box-attr (.box self) :top) (- (.origtop self)
                                       (mod (truncate (box-attr (.box self) :left)
                                                      (/bounce self))
                                            2))))

(defmethod gunpos ((self player))
  (list (+ (if (eql (.flip self) lgame::+sdl-flip-none+)
               (- (/gun-offset self))
               (/gun-offset self))
           (box-attr (.box self) :centerx))
        (box-attr (.box self) :top)))


(defclass alien (sprite add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform 13)
   (animcycle :accessor /animcycle :allocation :class :initform 12)

   ; NOTE: this is a BAD idea to have this class-allocated if you care about
   ; being able to load the game in Lisp, launch, finish the game, and
   ; re-launch. That's because you need to reset this reference on shutdown
   ; and you can only do that with an existing alien sprite. I do it,
   ; but you should consider a better way.
   ; (See also https://dirtypipe.cm4all.com/ for another reason re-use
   ; without re-initializing can lead to a security issue, too.)
   (image-frames :accessor /image-frames :allocation :class :initform (list))

   (facing :accessor .facing)
   (frame :accessor .frame)))

(defun random-choice (choices)
  (elt choices (random (length choices))))

(defmethod initialize-instance :after ((self alien) &key)
  (unless (/image-frames self)
    (setf (/image-frames self) (list (lgame.loader:get-texture "alien1.png")
                                     (lgame.loader:get-texture "alien2.png")
                                     (lgame.loader:get-texture "alien3.png"))))
  (setf (.image self) (first (/image-frames self))
        (.box self) (get-texture-box (.image self))
        (.facing self) (* (/speed self) (random-choice '(-1 1)))
        (.frame self) 0)
  (when (minusp (.facing self))
    (setf (box-attr (.box self) :right) (box-attr lgame:*screen-box* :right))))

(defmethod update ((self alien))
  (with-accessors ((box .box) (facing .facing) (frame .frame) (image .image)) self
    (move-box box (.facing self) 0)
    (unless (box-contains? lgame:*screen-box* box) ; alien reached the edge of screen
      (setf facing (- facing)
            (box-attr box :top) (1+ (box-attr box :bottom)))
      (clamp box lgame:*screen-box*))
    (incf frame) ; change frame image every animcycle frames
    (setf image (elt (/image-frames self) (mod (truncate frame (/animcycle self)) (length (/image-frames self)))))))


(defclass explosion (sprite add-groups-mixin)
  ((lifetime :accessor /lifetime :allocation :class :initform 12)
   (animcycle :accessor /animcycle :allocation :class :initform 3)

   (life :accessor .life))
  (:documentation "Explosion for both alien and player,
                   lasts by default 12 frames, 'animating' between
                   a flipped image/not every 3 frames."))

(defmethod initialize-instance :after ((self explosion) &key actor)
  (setf (.image self) (lgame.loader:get-texture "explosion1.png")
        (.box self) (get-texture-box (.image self))
        (.life self) (/lifetime self))
  (if actor
  (setf (box-attr (.box self) :center) (box-attr (.box actor) :center))))

(defmethod update ((self explosion))
  (decf (.life self))
  (if (zerop (mod (truncate (.life self) (/animcycle self)) 2))
      (setf (.flip self) lgame::+sdl-flip-none+)
      (setf (.flip self) lgame::+sdl-flip-horizontal+))
  (unless (plusp (.life self))
    (kill self)))


(defclass shot (sprite add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform -11)))

(defmethod initialize-instance :after ((self shot) &key pos)
  (setf (.image self) (lgame.loader:get-texture "shot.png")
        (.box self) (get-texture-box (.image self))
        (box-attr (.box self) :midbottom) pos))

(defmethod update ((self shot))
  (move-box (.box self) 0 (/speed self))
  (unless (plusp (box-attr (.box self) :top))
    (kill self)))

(defclass bomb (sprite add-groups-mixin)
  ((speed :accessor /speed :allocation :class :initform 9)))

(defmethod initialize-instance :after ((self bomb) &key alien)
  (setf (.image self) (lgame.loader:get-texture "bomb.png")
        (.box self) (get-texture-box (.image self)))
  (with-moved-box (moved (.box alien) 0 5)
    (setf (box-attr (.box self) :midbottom) (box-attr moved :midbottom))))

(defmethod update ((self bomb))
  (move-box (.box self) 0 (/speed self))
  (when (>= (box-attr (.box self) :bottom) (- (box-attr lgame:*screen-box* :bottom) 10))
    (make-instance 'explosion :actor self :groups (lgame.sprite:.groups self))
    (kill self)))


(defclass score (sprite add-groups-mixin)
  ((score :accessor .score :initform 0)
   (last-score :accessor .last-score :initform -1)))

;; Score is special in that it doesn't use a fixed image but instead its own
;; continuously created and destroyed texture that results from rendering text.
(defmethod initialize-instance :after ((self score) &key)
  (setf (.image self) nil)
  (update self)
  (setf (.box self) (get-texture-box (.image self)))
  (move-box (.box self) 10 (- (box-attr lgame:*screen-box* :bottom) 30)))

(defmethod update ((self score))
  (when (/= (.score self) (.last-score self))
    (setf (.last-score self) (.score self))
    (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 20))
           (msg (format nil "Score: ~a" (.score self)))
           (new-score-texture (lgame.font:render-text font msg 255 255 255)))
      (when (.image self) ; destroy previous score
        (lgame.texture:destroy-texture (.image self)))
      (setf (.image self) new-score-texture))))


(defun main (&aux background boom-sound shoot-sound music groups group-lists player score)
  (lgame:init)
  (lgame.loader:create-texture-loader *source-dir*)
  (lgame.display:create-centered-window "Aliens?!"(first +screen-size+) (second +screen-size+))
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size (first +screen-size+) (second +screen-size+))
  (sdl2:hide-cursor)

  (sdl2:pump-events) ;; needed for raise-window to work
  (lgame::sdl-raise-window lgame:*screen*)

  (let ((bg-tile (lgame.loader:get-texture "background.png")))
    (setf background (lgame.texture:create-sdl-texture lgame:*renderer* lgame::+sdl-pixelformat-rgba8888+ lgame::+sdl-textureaccess-target+
                                                       (first +screen-size+) (second +screen-size+)))
    (lgame.render:with-render-target background
      (loop for x from 0 below (first +screen-size+) by (lgame.texture:.width bg-tile) do
            (with-box-as-sdl-rect (r (make-box x 0 (lgame.texture:.width bg-tile) (lgame.texture:.height bg-tile)))
              (sdl2:render-copy lgame:*renderer* (lgame.texture:.sdl-texture bg-tile) :dest-rect r)))))

  (setf boom-sound (sdl2-mixer:load-wav (merge-pathnames "boom.wav" *source-dir*)))
  (setf shoot-sound (sdl2-mixer:load-wav (merge-pathnames "car_door.wav" *source-dir*)))
  (setf music (sdl2-mixer:load-music (merge-pathnames "house_lo.wav" *source-dir*)))
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
    (lgame.texture:destroy-texture background)
    ; to support running again in a repl, need to also clear alien's
    ; class-allocated image frames
    (let ((alien-class-proto (closer-mop:class-prototype (find-class 'alien))))
      (setf (/image-frames alien-class-proto) nil)) ; or use slot-value?

    ; may not need to be called because of sdl2-mixer's usage of autocollect...
    ;(sdl2-mixer:free-music music)
    ;(sdl2-mixer:free-chunk shoot-sound)
    ;(sdl2-mixer:free-chunk boom-sound)

    (lgame:quit)))

(defun game-tick (background boom-sound shoot-sound groups group-lists player score)
  (do-event (event)
    (if (or
          (= (event-type event) lgame::+sdl-quit+)
          (and (= (event-type event) lgame::+sdl-keydown+)
               (= (key-scancode event) lgame::+sdl-scancode-escape+)))
        (setf *running?* nil))
    (if (and (= (event-type event) lgame::+sdl-keyup+)
             (= (key-scancode event) lgame::+sdl-scancode-f+))
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

  (lgame.render:clear)
  (lgame.render:blit background nil)
  (draw (getf groups :all))
  (lgame.render:present)

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
               (< (lgame.sprite:sprite-count (getf groups :shots)) +max-shots+))
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
  (let ((shots-to-kill ()))
    (dolist (collisions (lgame.sprite:group-collide (getf groups :aliens) (getf groups :shots)))
      (let ((alien (car collisions))
            (shots (cdr collisions)))
        (sdl2-mixer:play-channel -1 boom-sound 0)
        (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor alien)
        (incf (.score score))
        (kill alien)
        (setf shots-to-kill (union shots-to-kill shots))))
    (mapc #'kill shots-to-kill)))

(defun handle-bombs-player-collisions (player groups group-lists boom-sound)
  (dolist (bomb (lgame.sprite:sprite-collide player (getf groups :bombs)))
    (sdl2-mixer:play-channel -1 boom-sound 0)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor player)
    (make-instance 'explosion :groups (getf group-lists :for-explosion) :actor bomb)
    (kill bomb)
    (kill player)
    ; dead
    (setf *running?* nil)))

(eval-when (:execute)
  (main))
