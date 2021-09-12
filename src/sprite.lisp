#|
WIP sprite module, inspired by pygame.sprite
The idea is to provide a base Sprite class that sprite objects can inherit from
and optionally override their own update and draw methods.
Additionally a simple Group class is provided to manage collections of sprites.

Perhaps later other kinds of groups or sprite types will be added.
|#

(in-package #:lgame)

(annot:enable-annot-syntax)

@export
(defgeneric update (sprite-or-group)
  (:documentation
    "Control the behavior of a Sprite, or each sprite in a Group"))

@export
(defgeneric draw (sprite-or-group)
  (:documentation
    "Render the Sprite image, or each sprite in a Group"))

@export
(defgeneric cleanup (sprite-or-group)
  (:documentation
    "Frees the Sprite's image and rect, or those of each sprite in a Group.
     Note that even if an image was loaded with the *texture-loader*, neither
     will double-free when unloading."))

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

@export-class
(defclass sprite ()
  ((image :accessor image-of :type (or null sdl2-ffi:sdl-texture))
   (rect :accessor rect-of :type (or null sdl2-ffi:sdl-rect))
   (angle :accessor angle-of :type double-float :initform 0.0d0
          :documentation "Angle in degrees applied to the rect when rendering, rotating it clockwise")
   (flip :accessor flip-of :type bit ;sdl2-ffi:sdl-renderer-flip
         :initform sdl2-ffi:+sdl-flip-none+)
   (groups :accessor groups-of :type list :initform (list) :documentation "List of groups containing this sprite"))
  (:documentation
    "A sprite object contains a reference to a texture, stored with the image attribute,
     and a rect attribute corresponding to the sprite's location and size.
     These are used by the default implementation of draw to copy the texture
     to the global renderer at the position and size of the rect.
     Additionally, a rotation angle and/or flip attribute can be given." ; should be a subclass?
    ))

(defmethod (setf angle-of) :after
  (new-value (self sprite))
  "Automatically convert new angle values to double-float if needed"
  (unless (typep new-value 'double-float)
    (setf (slot-value self 'angle) (coerce new-value 'double-float))))

@export-class
(defclass group ()
  ((sprites :accessor sprites-of :initarg :sprites :initform (list) :type list :documentation "List of Sprites that this Group contains")
   )
  (:documentation
    "A container for holding Sprites. The main benefit is as a proxy to call update/draw methods of each sprite in sequence.
    Additional methods to handle group membership are provided."
    ))

(defmethod update ((self sprite))
  "Default update for a sprite is a no-op"
  )

(defmethod draw ((self sprite))
  "Default draw uses the global renderer *renderer* to copy the image of the sprite
   onto the location defined by the rect of the sprite.
   If the angle is non-zero a rotation will be applied around the sprite's center based on its rect location.
   If flip is +sdl-flip-horizontal+ or +sdl-flip-vertical+ the image will be flipped accordingly."
  (sdl-render-copy-ex *renderer* (image-of self) nil (rect-of self) (angle-of self) nil (flip-of self)))

(defmethod cleanup ((self sprite))
  (sdl2:free-rect (rect-of self))
  (if (autowrap:valid-p (image-of self))
    (sdl2:destroy-texture (image-of self))))

(defmethod add-groups ((self sprite) &rest groups)
  (dolist (group groups)
    (add-sprites group self)))

(defmethod remove-groups ((self sprite) &rest groups)
  (dolist (group groups)
    (remove-sprites group self)))

(defmethod update ((self group))
  (dolist (sprite (sprites-of self))
    (update sprite)))

(defmethod draw ((self group))
  (dolist (sprite (sprites-of self))
    (draw sprite)))

(defmethod cleanup ((self group))
  (dolist (sprite (sprites-of self))
    (cleanup sprite)))

(defmethod add-sprites ((self group) &rest sprites)
  (setf (sprites-of self) (union (sprites-of self) sprites))
  (dolist (sprite sprites)
    (setf (groups-of sprite) (union (groups-of sprite) (list self)))))

(defmethod remove-sprites ((self group) &rest sprites)
  (setf (sprites-of self) (set-difference (sprites-of self) sprites))
  (dolist (sprite sprites)
    (setf (groups-of sprite) (set-difference (groups-of sprite) (list self)))))

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

