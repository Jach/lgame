(in-package #:lgame.box)

(defclass box ()
  ((min-x :initarg :min-x :accessor box-min-x :type real :initform 0)
   (min-y :initarg :min-y :accessor box-min-y :type real :initform 0)
   (max-x :initarg :max-x :accessor box-max-x :type real :initform 0)
   (max-y :initarg :max-y :accessor box-max-y :type real :initform 0))
  (:documentation "Represents an axis-aligned 2D box using min/max coordinates.
                   Intended to be useful where SDL_Rect would be, but allows for real-value
                   coordinates, and thus can serve dual-use for e.g. world-space position coordinates."))

(defun make-box (x y width height)
  "Creates a new Box from x, y, width, and height.
   The box has its top-left corner at the given x,y."
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
            (box-max-x b) (box-max-y b))))

(defun copy-box (b)
  (make-instance 'box
                 :min-x (box-min-x b)
                 :min-y (box-min-y b)
                 :max-x (box-max-x b)
                 :max-y (box-max-y b)))

(defmacro with-box-as-sdl-rect ((rect-name box) &body body)
  "Creates a new SDL_Rect on the stack bound to RECT-NAME and populates it with values from the Box.
   The values are rounded to meet SDL_Rect's type requirements, be cautious
   if using floats."
  (let ((sz (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-rect))))
    `(cffi:with-foreign-pointer (,rect-name ,sz)
       (setf (sdl2:rect-x ,rect-name) (round (box-x ,box))
             (sdl2:rect-y ,rect-name) (round (box-y ,box))
             (sdl2:rect-width ,rect-name) (round (box-width ,box))
             (sdl2:rect-height ,rect-name) (round (box-height ,box)))
       ,@body)))

(defmethod box-x ((b box))
  (box-min-x b))
(defmethod box-y ((b box))
  (box-min-y b))
(defmethod box-width ((b box))
  (- (box-max-x b) (box-min-x b)))
(defmethod box-height ((b box))
  (- (box-max-y b) (box-min-y b)))

(defmethod (setf box-x) (new-val (b box))
  (let ((current-width (box-width b)))
    (setf (box-min-x b) new-val
          (box-max-x b) (+ new-val current-width))))

(defmethod (setf box-y) (new-val (b box))
  (let ((current-height (box-height b)))
    (setf (box-min-y b) new-val
          (box-max-y b) (+ new-val current-height))))

(defmethod (setf box-width) (new-val (b box))
  (setf (box-max-x b) (+ (box-min-x b) new-val)))

(defmethod (setf box-height) (new-val (b box))
  (setf (box-max-y b) (+ (box-min-y b) new-val)))

(defun midway (val size-val)
  (+ val (/ size-val 2)))

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
  (let ((min-x (box-min-x box))
        (min-y (box-min-y box))
        (max-x (box-max-x box))
        (max-y (box-max-y box)))
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
      (:min-x (setf (box-min-x box) value))
      (:min-y (setf (box-min-y box) value))
      (:max-x (setf (box-max-x box) value)) ; This changes width from min-x
      (:max-y (setf (box-max-y box) value)) ; This changes height from min-y
      (:top (setf (box-y box) value))
      (:left (setf (box-x box) value))
      (:bottom
        (let ((current-height (box-height box)))
          (setf (box-max-y box) value)
          (setf (box-min-y box) (- value current-height))))
      (:right
        (let ((current-width (box-width box)))
          (setf (box-max-x box) value)
          (setf (box-min-x box) (- value current-width))))
      (:centerx
        (let ((half-w (/ w 2)))
          (setf (box-min-x box) (- value half-w)
                (box-max-x box) (+ value half-w))))
      (:centery
        (let ((half-h (/ h 2)))
          (setf (box-min-y box) (- value half-h)
                (box-max-y box) (+ value half-h))))
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


;; Basic Collision/Containment Methods

(defun box-contains-point? (box px py)
  "Checks if the point (px, py) is contained within the box.
   The right and bottom edges are typically exclusive for point containment."
  (and (>= px (box-min-x box))
       (<  px (box-max-x box))
       (>= py (box-min-y box))
       (<  py (box-max-y box))))

(defun boxes-intersect? (b1 b2)
  "Checks if two boxes, b1 and b2, intersect (overlap)."
  (declare (type box b1 b2))
  (not (or (< (box-max-x b1) (box-min-x b2)) ; b1 is to the left of b2
           (< (box-max-x b2) (box-min-x b1)) ; b2 is to the left of b1
           (< (box-max-y b1) (box-min-y b2)) ; b1 is above b2
           (< (box-max-y b2) (box-min-y b1)) ; b2 is above b1
           )))

(defun boxes-collide? (b1 b2)
  "Alias for boxes-intersect?."
  (boxes-intersect? b1 b2))
