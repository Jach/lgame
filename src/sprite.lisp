(in-package #:lgame.sprite)

(annot:enable-annot-syntax)

;;;; Protocol for sprites and groups

@export
(defgeneric update (sprite-or-group)
  (:documentation
    "Control the behavior of a Sprite, or each sprite in a Group"))

@export
(defgeneric draw (sprite-or-group)
  (:documentation
    "Render the Sprite image, or each sprite in a Group"))

@export
(defgeneric kill (sprite)
  (:documentation
    "Removes the Sprite from all groups it belongs to.
     Note it does not free any resources like the sprite rect,
     if you plan to manage sprites entirely with groups
     and expect a kill to cleanup, you might want to create
     an :after method for kill on your sprite to handle that,
     or also inherit the 'cleaned-on-kill-mixin which defines
     such a method for you."))

@export
(defgeneric cleanup (sprite-or-group)
  (:documentation
    "Frees the Sprite's rect, or those of each sprite in a Group.
     Note it does not free the sprite's image, which is fine if it
     was loaded with the texture-loader as it will be freed when textures
     are unloaded, but the user should do that themselves if loaded another way."))

@export
(defgeneric add-groups (sprite &rest groups)
  (:documentation
    "Add the sprite to any given groups it's not already a member of."))

@export
(defgeneric remove-groups (sprite &rest groups)
  (:documentation
    "Remove the sprite from any number of given groups, if it exists."))

@export
(defgeneric add-sprites (group &rest sprites)
  (:documentation
     "Add any number of sprites to the given group."))

@export
(defgeneric remove-sprites (group &rest sprites)
  (:documentation
     "Given a group, remove any number of sprites that exist in the group."))

@export
(defgeneric map-sprite (function group)
  (:documentation
    "Given a group, iterate over all sprites in its collection and call
     the given function with the sprite as its argument."))

@export
(defmacro do-sprite ((sprite-var group) &body body)
  "Iterate over each sprite in the group, binding to
   sprite-var. Ultimately just a wrapper around map-sprite."
  `(map-sprite (lambda (,sprite-var) ,@body) ,group))

@export
(defgeneric remove-all-sprites (group &optional cleanup?)
  (:documentation
    "Given a group, remove all its sprites.
     If optional cleanup? is T, calls cleanup first."))

@export
(defgeneric sprite-count (group)
  (:documentation
    "Returns how many sprites are part of this group"))

@export
(defgeneric empty? (group)
  (:documentation
    "True if a group contains sprites, nil otherwise."))

@export
(defgeneric sprite-collide (sprite group)
  (:documentation
    "Checks to see if the .rect of the sprite collides with any of the rects of the sprites within group.
     Returns a list of such colliding sprites."))

@export
(defgeneric group-collide (group1 group2)
  (:documentation
    "Checks to see if any sprites within group1 collide with any sprites in group2 based on each sprite's .rect rect-colliding.
     Returns a list of collisions in the form ((sprite-from-group1 . (collided-sprite-from-group2 ...))
                                               (other-sprite-from-group1 . (collided-sprite-from-group2 ...))
                                               ...)
     Note: if a sprite from group 2 collides with multiple sprites from group 1, it will be represented multiple times.
     Therefore, do not call methods on sprites from group 2 (like cleaning them up) until you have filtered out
     duplicates to avoid calling a method multiple times and creating errors like a double-free."))

@export
(defgeneric group-query-class (group class)
  (:documentation
    "Returns all sprites inside group that are of the type class."))

;;;; Sprite class

@export-class
(defclass sprite ()
  ((image :accessor .image :type (or null sdl2-ffi:sdl-texture))
   (rect :accessor .rect :type (or null sdl2-ffi:sdl-rect))
   (angle :accessor .angle :type double-float :initform 0.0d0
          :documentation "Angle in degrees applied to the rect when rendering, rotating it clockwise")
   (flip :accessor .flip :type bit ;sdl2-ffi:sdl-renderer-flip
         :initform sdl2-ffi:+sdl-flip-none+)

   (alive? :accessor .alive? :type boolean :initform T :documentation "Has this sprite been killed yet?")
   (groups :accessor .groups :type list :initform (list) :documentation "List of groups containing this sprite"))
  (:documentation
    "A sprite object contains a reference to a texture, stored with the image attribute,
     and a rect attribute corresponding to the sprite's location and size.
     These are used by the default implementation of draw to copy the texture
     to the global renderer at the position and size of the rect.
     Additionally, a rotation angle and/or flip attribute can be given." ; should be a subclass, along with alive?/groups?
    ))

(defmethod print-object ((o sprite) stream)
  (let ((r (if (slot-boundp o 'rect)
               (.rect o)
               "<unbound>")))
    (print-unreadable-object (o stream :type t :identity t)
      (format stream "rect ~a groups-length ~a alive? ~a"
              r (length (.groups o)) (.alive? o)))))

(defmethod (setf .angle) :after
  (new-value (self sprite))
  "Automatically convert new angle values to double-float if needed"
  (unless (typep new-value 'double-float)
    (setf (slot-value self 'angle) (coerce new-value 'double-float))))

;;;; Sprite mixins

@export-class
(defclass cleaned-on-kill-mixin ()
  ()
  (:documentation
    "A sprite class mixin that provides a default :after method on 'kill
     which will automatically call the sprite's 'cleanup method, unless
     that sprite is still '.alive?"))

@export-class
(defclass add-groups-mixin ()
  ()
  (:documentation
    "A sprite class mixin that provides an extra :after constructor that
     accepts a :groups argument and will apply 'add-groups to it,
     adding the constructed sprite to the list of passed groups.
     Can also be used with a single group to be added."))

(defmethod initialize-instance :after ((self add-groups-mixin) &key groups)
  (if (atom groups)
      (add-groups self groups)
      (apply #'add-groups self groups)))


;;;; Group classes

@export-class
(defclass group ()
  ((sprites :accessor .sprites :initarg :sprites :initform (list) :type list :documentation "Collection of Sprites that this Group contains")
   )
  (:documentation
    "A container for holding Sprites. The main benefit is as a proxy to call update/draw methods of each sprite in sequence.
    Additional methods to handle group membership are provided.
    Note that the order of the sprites is not guaranteed when adding/removing, which affects render order. Consider one of the
    Group subclasses if you need different behavior.
    If it is desired to iterate over the sprites yourself, it is recommended to use the provided map-sprite function
    or do-sprite macro rather than accessing the sprites slot directly."
    ))

(defmethod print-object ((o group) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "sprites ~a" (.sprites o))))

@export-class
(defclass ordered-group (group)
  ((seen-map :initform (make-hash-table :test #'equal) :documentation "Collection of seen sprites to avoid duplicate entries"))
  (:documentation
    "A group container that retains the order of existing sprites as you add/remove them, so you can count on
     draws to happen in order that you added sprites."))

@export-class
(defclass group-single (group)
  ((sprite :accessor .sprite :initarg :sprite :initform nil :type (or null sprite)))
  (:documentation
    "A group that can only contain a single sprite, or nothing.
     When a new sprite is added, the old one is removed.
     The single sprite is setf-able."))

(defmethod (setf .sprite) :after (new-value (self group-single))
  (add-sprites self new-value))

(defmethod print-object ((o group-single) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "sprite ~a" (.sprite o))))


;;;; Methods for Sprites

(defmethod update ((self sprite))
  "Default update for a sprite is a no-op"
  )

(defmethod draw ((self sprite))
  "Default draw uses the global renderer *renderer* to copy the image of the sprite
   onto the location defined by the rect of the sprite.
   If the angle is non-zero a rotation will be applied around the sprite's center based on its rect location.
   If flip is +sdl-flip-horizontal+ or +sdl-flip-vertical+ the image will be flipped accordingly."
  (sdl2::check-rc
    (sdl2-ffi.functions:sdl-render-copy-ex lgame:*renderer* (.image self) nil (.rect self) (.angle self) nil (.flip self))))

(defmethod kill ((self sprite))
  (setf (.alive? self) nil)
  (apply #'remove-groups self (.groups self)))

(defmethod kill :after ((self cleaned-on-kill-mixin))
  (unless (.alive? self)
    (cleanup self)))

(defmethod cleanup ((self sprite))
  (sdl2:free-rect (.rect self)))

(defmethod add-groups ((self sprite) &rest groups)
  (dolist (group groups)
    (add-sprites group self)))

(defmethod remove-groups ((self sprite) &rest groups)
  (dolist (group groups)
    (remove-sprites group self)))


;;;; Methods for default group

(defmethod map-sprite (function (self group))
  (mapcar function (.sprites self)))

(defmethod update ((self group))
  (map-sprite #'update self)
  (values))

(defmethod draw ((self group))
  (map-sprite #'draw self)
  (values))

(defmethod cleanup ((self group))
  (map-sprite #'cleanup self)
  (values))

;; todo, don't use lists as sets...
(defmethod add-sprites ((self group) &rest sprites)
  (setf (.sprites self) (union (.sprites self) sprites))
  (dolist (sprite sprites)
    (setf (.groups sprite) (union (.groups sprite) (list self)))))

(defmethod remove-sprites ((self group) &rest sprites)
  (setf (.sprites self) (set-difference (.sprites self) sprites))
  (dolist (sprite sprites)
    (setf (.groups sprite) (set-difference (.groups sprite) (list self)))))

(defmethod remove-all-sprites ((group group) &optional cleanup?)
  (when cleanup?
    (cleanup group))
  (setf (.sprites group) nil))

(defmethod sprite-count ((self group))
  (length (.sprites self)))

(defmethod empty? ((self group))
  (null (.sprites self)))

(defmethod sprite-collide ((sprite sprite) (group group))
  (let ((collisions (list)))
    (do-sprite (group-sprite group)
      (when (lgame.rect:collide-rect? (.rect sprite) (.rect group-sprite))
        (push group-sprite collisions)))
    collisions))

(defmethod group-collide ((group1 group) (group2 group))
  (let ((all-collisions (list)))
    (do-sprite (sprite1 group1)
      (let ((sprite1-collisions (list)))
        (do-sprite (sprite2 group2)
          (when (lgame.rect:collide-rect? (.rect sprite1) (.rect sprite2))
            (push sprite2 sprite1-collisions)))
        (when sprite1-collisions
          (push (cons sprite1 sprite1-collisions) all-collisions))))
    all-collisions))

(defmethod group-query-class ((group group) class)
  (let ((sprites (list)))
    (do-sprite (sprite group)
      (when (typep sprite class)
        (push sprite sprites)))
    sprites))


;;;; Methods for ordered-group

(defmethod add-sprites ((self ordered-group) &rest sprites)
  (let ((unique-sprites (remove-if (lambda (v) (gethash v (slot-value self 'seen-map)))
                                   sprites)))
    (setf (.sprites self) (append (.sprites self) unique-sprites))
    (dolist (sprite unique-sprites)
      (setf (gethash sprite (slot-value self 'seen-map)) T)
      (setf (.groups sprite) (union (.groups sprite) (list self))))))

(defmethod remove-sprites ((self ordered-group) &rest sprites)
  (dolist (sprite sprites)
    (setf (gethash sprite (slot-value self 'seen-map)) nil)
    (setf (.sprites self) (remove sprite (.sprites self) :test #'equal))
    (setf (.groups sprite) (set-difference (.groups sprite) (list self)))))


;;;; Methods for group-single

(defmethod add-sprites ((self group-single) &rest sprites)
  (let ((sprite (car (last sprites)))) ; only expect single sprite, but if passed multiple only use last one
    (setf (slot-value self 'sprite) sprite)
    (setf (.groups sprite) (union (.groups sprite) (list self)))))

(defmethod remove-sprites ((self group-single) &rest sprites)
  (dolist (sprite sprites)
    (when (equal sprite (.sprite self))
      (setf (.groups sprite) (remove self (.groups sprite) :test #'equal))
      (setf (slot-value self 'sprite) nil))))



;; example using https://opengameart.org/content/various-gem-stone-animations
;;(load-spritesheet (format nil "~a/sapphirespinning.png" +sprites-dir+)
;;                  '(:start-x 0
;;                    :start-y 6
;;                    :cols 7
;;                    :rows 6
;;                    :ignore-last 3
;;                    :width 30
;;                    :height 18
;;                    :padding-x 0
;;                    :padding-y 12))

#| Revisit once I add animations again
(defun load-spritesheet (file frame-data)
  "Given a sprite sheet file and frame-data for the sheet, loads the spritesheet into an array
   of textures with each cell being a frame.
   Alternative to look into: just loading the spritesheet into a single texture like normal,
   and using source-rects in the frame data to pick off which frame to use..."
  (let* ((surface (sdl2-image:load-image file))
         (texture (sdl-create-texture-from-surface *renderer* surface))
         (textures (make-array (- (* (getf frame-data :cols) (getf frame-data :rows))
                                  (getf frame-data :ignore-last))
                               :fill-pointer 0)))
    (sdl-free-surface surface)
    (with-rect (clip-src (getf frame-data :start-x) (getf frame-data :start-y)
                         (getf frame-data :width) (getf frame-data :height))
      (loop for row below (getf frame-data :rows) do
            (loop for col below (getf frame-data :cols)
                  :until (= (array-total-size textures)
                            (- (* row col) (getf frame-data :ignore-last)))
                  :do
                  (let ((tex (sdl-create-texture *renderer* sdl2-ffi:+sdl-pixelformat-rgba8888+
                                                 sdl2-ffi:+sdl-textureaccess-target+
                                                 (getf frame-data :width)
                                                 (getf frame-data :height))))
                    ;(sdl-set-texture-blend-mode tex sdl2-ffi:+sdl-blendmode-blend+) ; consider if tex has stuff on it already
                    (sdl-set-render-target *renderer* tex)
                    (sdl-render-copy *renderer* texture clip-src nil)
                    (vector-push tex textures))
                  (move-rect clip-src
                             (+ (getf frame-data :width)
                                (getf frame-data :padding-x))
                             0))
            (setf (sdl2:rect-x clip-src) (getf frame-data :start-x))
            (move-rect clip-src
                       0
                       (+ (getf frame-data :height)
                          (getf frame-data :padding-y)))
                  ))
    (sdl-set-render-target *renderer* nil)
    (sdl-destroy-texture texture)
    textures))

;; example using https://opengameart.org/content/dragon-fully-animated
;(load-spritedir (format nil "~a/../Dragon - Fully Animated/Death/" +sprites-dir+))

(defun load-spritedir (dir)
  "Given a folder of ordered sprite frames, loads them into textures and returns them in an array."
  (let* ((files (uiop:directory-files dir))
         (textures (make-array (length files) :fill-pointer 0)))
    (dolist (file files)
      (let* ((surface-orig (sdl2-image:load-image file))
             ;(surface (trim-surface surface-orig :max-padding-left 140 :max-padding-top 46))
             (texture (sdl-create-texture-from-surface *renderer* surface-orig)))
        (sdl-free-surface surface-orig)
        ;(sdl-free-surface surface)
        (vector-push texture textures)))
    textures))

(defun trim-surface (surface &key max-padding-left max-padding-top)
  "Given a surface, check each side for repeating rows/columns of identical pixels,
   and return a new surface with those sides trimmed off. Depending on how much
   paddding is around the surface this can greatly decrease the final rect size
   before converting to a texture. Though if you're using this for a series of
   sprites in an animation, be wary about the padding being inconsistent frame-by-frame.
   This affects results if any of the rects are differently sized (position or width/height)

   To help with that, allow passing in max-trim for each side..

   Assumes input surface is in the 32-bit rgba format

   Assumes input surface has some pixels that differ"
  (let* ((pixels (sdl2:surface-pixels surface))
         (w (sdl2:surface-width surface))
         (h (sdl2:surface-height surface))
         (prev-pixel (cffi:mem-aref pixels :uint32 0))
         (padding-top 0)
         (padding-left 0)
         (padding-right 0)
         (padding-bottom 0))
    (with-rect (trimmed 0 0 0 0)
      ; figure out top padding
      (setf padding-top
            (block outer
                   (loop for row below h do
                         (loop for col below w do
                               (let ((pixel (cffi:mem-aref pixels :uint32 (+ col (* row w)))))
                                 (if (/= pixel prev-pixel)
                                     (return-from outer row))
                                 (setf prev-pixel pixel))))
                   (1- h)))
      (if (and max-padding-top
               (> padding-top max-padding-top))
          (setf padding-top max-padding-top))

      (setf prev-pixel (cffi:mem-aref pixels :uint32 0))
      (setf (sdl2:rect-y trimmed) padding-top)
      ; figure out left padding
      (setf padding-left
            (block outer
                   (loop for col below w do
                         (loop for row below h do
                               (let ((pixel (cffi:mem-aref pixels :uint32 (+ col (* row w)))))
                                 (if (/= pixel prev-pixel)
                                     (return-from outer col))
                                 (setf prev-pixel pixel))))
                   (1- w)))
      (if (and max-padding-left
               (> padding-left max-padding-left))
          (setf padding-left max-padding-left))

      (setf prev-pixel (cffi:mem-aref pixels :uint32 0))
      (setf (sdl2:rect-x trimmed) padding-left)
      ; figure out right padding
      ;(setf padding-right
      ;      (block outer
      ;             (loop for col from (1- w) downto 0 do
      ;                   (loop for row below h do
      ;                         (let ((pixel (cffi:mem-aref pixels :uint32 (+ col (* row w)))))
      ;                           (if (/= pixel prev-pixel)
      ;                               (return-from outer (1+ (- w col))))
      ;                           (setf prev-pixel pixel))))
      ;             (1- w)))

      (setf prev-pixel (cffi:mem-aref pixels :uint32 0))
      (setf (sdl2:rect-width trimmed) (- w padding-left padding-right))
      ; figure out bottom padding
      ;(setf padding-bottom
      ;      (block outer
      ;             (loop for row from (1- h) downto 0 do
      ;                   (loop for col below w do
      ;                         (let ((pixel (cffi:mem-aref pixels :uint32 (+ col (* row w)))))
      ;                           (if (/= pixel prev-pixel)
      ;                               (return-from outer (1+ (- h row))))
      ;                           (setf prev-pixel pixel))))
      ;             (1- h)))

      (setf (sdl2:rect-height trimmed) (- h padding-top padding-bottom))
      ;(format t "~a~%" (rect-string trimmed))

      (let ((dst-surface (sdl-create-rgb-surface (plus-c:c-ref surface sdl2-ffi:sdl-surface :flags)
                                                 (sdl2:rect-width trimmed)
                                                 (sdl2:rect-height trimmed)
                                                 (plus-c:c-ref surface sdl2-ffi:sdl-surface :format :bits-per-pixel)
                                                 (plus-c:c-ref surface sdl2-ffi:sdl-surface :format :Rmask)
                                                 (plus-c:c-ref surface sdl2-ffi:sdl-surface :format :Gmask)
                                                 (plus-c:c-ref surface sdl2-ffi:sdl-surface :format :Bmask)
                                                 (plus-c:c-ref surface sdl2-ffi:sdl-surface :format :Amask))))
        (sdl2:blit-surface surface trimmed dst-surface nil)
        dst-surface))))
|#
