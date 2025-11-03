(in-package #:lgame.box)

(defclass box ()
  ((min-x :initarg :min-x :accessor .min-x :type real :initform 0)
   (min-y :initarg :min-y :accessor .min-y :type real :initform 0)
   (max-x :initarg :max-x :accessor .max-x :type real :initform 0)
   (max-y :initarg :max-y :accessor .max-y :type real :initform 0))
  (:documentation "Represents an axis-aligned 2D box using min/max coordinates.
                   Intended to be useful where SDL_Rect would be, but allows for real-value
                   coordinates, and thus can serve dual-use for e.g. world-space position coordinates."))

(defun make-box (x y width height)
  "Creates a new Box from x, y, width, and height.
   The box has its top-left corner at the given x,y.
   Width and height must be positive."
  (declare (type (real 0) width height))
  (make-instance 'box
                 :min-x x
                 :min-y y
                 :max-x (+ x width)
                 :max-y (+ y height)))

(defun make-box-from-minmax (min-x min-y max-x max-y)
 "Creates a new BOX directly from min/max coordinates.
  The min-x and min-y define the top-left of the box."
 (make-instance 'box :min-x min-x :min-y min-y :max-x max-x :max-y max-y))

(defmethod print-object ((b box) stream)
  (print-unreadable-object (b stream :type t :identity t)
    (format stream "X:~A Y:~A W:~A H:~A MAX-X:~A MAX-Y:~A"
            (box-x b) (box-y b)
            (box-width b) (box-height b)
            (.max-x b) (.max-y b))))

(defun copy-box (b)
  (make-instance 'box
                 :min-x (.min-x b)
                 :min-y (.min-y b)
                 :max-x (.max-x b)
                 :max-y (.max-y b)))

(defmethod box-x ((b box))
  (.min-x b))

(defmethod box-y ((b box))
  (.min-y b))

(defmethod box-width ((b box))
  (- (.max-x b) (.min-x b)))

(defmethod box-height ((b box))
  (- (.max-y b) (.min-y b)))

(defmethod (setf box-x) (new-val (b box))
  (let ((current-width (box-width b)))
    (setf (.min-x b) new-val
          (.max-x b) (+ new-val current-width))))

(defmethod (setf box-y) (new-val (b box))
  (let ((current-height (box-height b)))
    (setf (.min-y b) new-val
          (.max-y b) (+ new-val current-height))))

(defmethod (setf box-width) (new-val (b box))
  (setf (.max-x b) (+ (.min-x b) new-val)))

(defmethod (setf box-height) (new-val (b box))
  (setf (.max-y b) (+ (.min-y b) new-val)))

(defun midway (val size-val)
  (+ val (/ size-val 2)))

;;; Fancy getter/setter for locations on the box

(deftype box-attr-keyword ()
  '(member :x :y :width :height
           :min-x :min-y :max-x :max-y
           :top :left :bottom :right
           :topleft :bottomleft :topright :bottomright
           :midtop :midleft :midbottom :midright
           :center :centerx :centery
           :size))

(defun box-attr (box attr-keyword)
  "Query a box attribute with an intuitive keyword name,
   defined by the box-attr-keyword type. If a name gives
   two values, a list of (x y) or (w h) will be returned."
  (declare (type box-attr-keyword attr-keyword))
  (let ((min-x (.min-x box))
        (min-y (.min-y box))
        (max-x (.max-x box))
        (max-y (.max-y box)))
    (let ((w (- max-x min-x))
          (h (- max-y min-y)))
      (ecase attr-keyword
        (:x min-x)
        (:y min-y)
        (:width w)
        (:height h)
        (:min-x min-x)
        (:min-y min-y)
        (:max-x max-x)
        (:max-y max-y)
        (:top min-y)
        (:left min-x)
        (:bottom max-y)
        (:right max-x)
        (:centerx (midway min-x w))
        (:centery (midway min-y h))
        (:topleft (list min-x min-y))
        (:bottomleft (list min-x max-y))
        (:topright (list max-x min-y))
        (:bottomright (list max-x max-y))
        (:midtop (list (midway min-x w) min-y))
        (:midleft (list min-x (midway min-y h)))
        (:midbottom (list (midway min-x w) max-y))
        (:midright (list max-x (midway min-y h)))
        (:center (list (midway min-x w) (midway min-y h)))
        (:size (list w h))))))

(defun (setf box-attr) (value box attr-keyword)
  "Set a box attribute using an intuitive keyword name.
   When a keyword implies two values (e.g., :topleft, :center, :size),
   VALUE is expected to be a sequence of two numbers like (x y) or (w h)."
  (declare (type box-attr-keyword attr-keyword))
  (let ((w (box-width box))
        (h (box-height box)))
    (ecase attr-keyword
      (:x (setf (box-x box) value))
      (:y (setf (box-y box) value))
      (:width (setf (box-width box) value))
      (:height (setf (box-height box) value))
      (:min-x (setf (.min-x box) value))
      (:min-y (setf (.min-y box) value))
      (:max-x (setf (.max-x box) value)) ; This changes width from min-x
      (:max-y (setf (.max-y box) value)) ; This changes height from min-y
      (:top (setf (box-y box) value))
      (:left (setf (box-x box) value))
      (:bottom
        (let ((current-height (box-height box)))
          (setf (.max-y box) value)
          (setf (.min-y box) (- value current-height))))
      (:right
        (let ((current-width (box-width box)))
          (setf (.max-x box) value)
          (setf (.min-x box) (- value current-width))))
      (:centerx
        (let ((half-w (/ w 2)))
          (setf (.min-x box) (- value half-w)
                (.max-x box) (+ value half-w))))
      (:centery
        (let ((half-h (/ h 2)))
          (setf (.min-y box) (- value half-h)
                (.max-y box) (+ value half-h))))
      (:topleft
        (setf (box-attr box :left) (elt value 0)
              (box-attr box :top) (elt value 1)))
      (:bottomleft
        (setf (box-attr box :left) (elt value 0)
              (box-attr box :bottom) (elt value 1)))
      (:topright
        (setf (box-attr box :right) (elt value 0)
              (box-attr box :top) (elt value 1)))
      (:bottomright
        (setf (box-attr box :right) (elt value 0)
              (box-attr box :bottom) (elt value 1)))
      (:midtop
        (setf (box-attr box :centerx) (elt value 0)
              (box-attr box :top) (elt value 1)))
      (:midleft
        (setf (box-attr box :left) (elt value 0)
              (box-attr box :centery) (elt value 1)))
      (:midbottom
        (setf (box-attr box :centerx) (elt value 0)
              (box-attr box :bottom) (elt value 1)))
      (:midright
        (setf (box-attr box :right) (elt value 0)
              (box-attr box :centery) (elt value 1)))
      (:center
        (setf (box-attr box :centerx) (elt value 0)
              (box-attr box :centery) (elt value 1)))
      (:size
        (setf (box-width box) (elt value 0)
              (box-height box) (elt value 1)))))
  value)


;;; Basic Collision/Containment Methods

(defun box-contains-point? (box px py)
  "Checks if the point (px, py) is contained within the box.
   The right and bottom edges are typically exclusive for point containment."
  (and (>= px (.min-x box))
       (<  px (.max-x box))
       (>= py (.min-y box))
       (<  py (.max-y box))))

(defun boxes-intersect? (b1 b2)
  "Checks if two boxes, b1 and b2, intersect (overlap)."
  (declare (type box b1 b2))
  (not (or (< (.max-x b1) (.min-x b2)) ; b1 is to the left of b2
           (< (.max-x b2) (.min-x b1)) ; b2 is to the left of b1
           (< (.max-y b1) (.min-y b2)) ; b1 is above b2
           (< (.max-y b2) (.min-y b1)) ; b2 is above b1
           )))

(defun boxes-collide? (b1 b2)
  "Alias for boxes-intersect?."
  (boxes-intersect? b1 b2))

(defun box-contains? (b1 b2)
  "Does box b1 completely contain box b2?
   Boxes that merely touch at the edges are considered contained.
   The only exception is degenerate boxes (zero width or height),
   where containment requires b1 to extend strictly past b2's
   collapsed edge rather than just meeting it."
  (and (<= (.min-x b1) (.min-x b2))
       (>  (.max-x b1) (.min-x b2)) ; degenerate width check
       (>= (.max-x b1) (.max-x b2))
       (<= (.min-y b1) (.min-y b2))
       (>  (.max-y b1) (.min-y b2)) ; degenerate height check
       (>= (.max-y b1) (.max-y b2))))

(defun box-properly-contains? (b1 b2)
  "Does box b1 'properly' contain box b2?
   This is like box-contains? except that sharing edges does NOT count:
   b2 must lie entirely inside b1's interior along both axes."
  (and (< (.min-x b1) (.min-x b2))
       (< (.max-x b2) (.max-x b1))
       (< (.min-y b1) (.min-y b2))
       (< (.max-y b2) (.max-y b1))))

;;; Box mutation

(defun move-box (box x y)
  "Updates the box's position by the given x and y coordinate amounts."
  (incf (.min-x box) x)
  (incf (.max-x box) x)
  (incf (.min-y box) y)
  (incf (.max-y box) y))

(defun set-box (box &key x y w h)
  "Updates any of the provided box properties with new values."
  (when x (setf (box-x box) x))
  (when y (setf (box-y box) y))
  (when w (setf (box-width box) w))
  (when h (setf (box-height box) h)))

(defun clamp (box1 box2)
  "Updates the x, y position of box1 so that it is clamped within the dimensions of box2.
   In other words, box1 is moved to be inside box2.
   If box 1 is bigger than box 2, along an axis, then that axis is adjusted to share the same center as box 2."
  ; handle x first
  (cond
    ((>= (box-attr box1 :width) (box-attr box2 :width)) ; box1 too wide to fit inside, so center x around box2's center
     (setf (box-attr box1 :centerx) (box-attr box2 :centerx)))
    ((< (box-attr box1 :min-x) (box-attr box2 :min-x)) ; box1's left edge is outside box2's left edge
     (setf (box-attr box1 :left) (box-attr box2 :left)))
    ((> (box-attr box1 :right) (box-attr box2 :right)) ; right edge is outside
     (setf (box-attr box1 :right) (box-attr box2 :right))))
  ; same thing for y
  (cond
    ((>= (box-attr box1 :height) (box-attr box2 :height))
     (setf (box-attr box1 :centery) (box-attr box2 :centery)))
    ((< (box-attr box1 :min-y) (box-attr box2 :min-y))
     (setf (box-attr box1 :top) (box-attr box2 :top)))
    ((> (box-attr box1 :bottom) (box-attr box2 :bottom))
     (setf (box-attr box1 :bottom) (box-attr box2 :bottom))))
  box1)

(defun inflate-box (box width-change height-change)
  "Inflates/deflates the given box by changed width/height around the center.
   Thus if the change is 2 pixels wider, the top-left corner will move left
   by one and top right corner move right by one with the overall width increasing by two."
  (let ((dx (if (and (integerp width-change) (evenp width-change))
                (ash width-change -1)
                (* 0.5 width-change)))
        (dy (if (and (integerp height-change) (evenp height-change))
                (ash height-change -1)
                (* 0.5 height-change))))
    (decf (.min-x box) dx)
    (incf (.max-x box) dx)
    (decf (.min-y box) dy)
    (incf (.max-y box) dy))
  box)

;;; Macros

(defmacro with-moved-box((box source-box moved-x moved-y) &body body)
  "Binds BOX to a new copy of SOURCE-BOX but with the box moved by MOVED-X and MOVED-Y."
  (let ((src-box (gensym)))
    `(let* ((,src-box ,source-box)
            (,box (make-box (+ ,moved-x (box-x ,src-box)) (+ ,moved-y (box-y ,src-box)) (box-width ,src-box) (box-height ,src-box))))
       ,@body)))

;;; SDL-dependent code follows

(defmacro with-box-as-sdl-rect ((rect box &optional (truncate? nil)) &body body)
  "Binds RECT-NAME to a newly created SDL_Rect on the stack and populates its x,y,w,h with values from the given box.
   An optional argument TRUNCATE? can be passed. If it is the immediate compile-time value T, then when converting
   the box's values, which are real numbers, to SDL's unsigned int values, truncation will be used.
   Otherwise the default behavior is rounding."
  (let ((sz (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-rect)))
        (conversion-fn (if truncate? 'truncate 'round))
        (src-box (gensym))
        (rect-ptr (gensym)))
    `(let ((,rect nil)
           (,src-box ,box))
       (cffi:with-foreign-pointer (,rect-ptr ,sz)
         (when ,src-box
           (setf (sdl2:rect-x ,rect-ptr) (,conversion-fn (box-x ,src-box))
                 (sdl2:rect-y ,rect-ptr) (,conversion-fn (box-y ,src-box))
                 (sdl2:rect-width ,rect-ptr) (,conversion-fn (box-width ,src-box))
                 (sdl2:rect-height ,rect-ptr) (,conversion-fn (box-height ,src-box)))
           (setf ,rect ,rect-ptr)
           ,@body)))))

(defun get-texture-box (texture &key (x 0) (y 0))
  "Returns a box object representing the bounding box of the passed 2D texture, initially positioned with top-left x, y = 0, 0 unless position overrides are given."
  (make-box x y (lgame.texture:.width texture) (lgame.texture:.height texture)))
