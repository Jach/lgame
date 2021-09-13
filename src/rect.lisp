(in-package #:lgame.rect)

(annot:enable-annot-syntax)

@export
(defmacro with-rect ((rect x y w h) &body body)
  "Helper macro if one needs a stack rect"
  (let ((sz (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-rect))))
  `(cffi:with-foreign-pointer (,rect ,sz)
     (setf (sdl2:rect-x ,rect) ,x
           (sdl2:rect-y ,rect) ,y
           (sdl2:rect-width ,rect) ,w
           (sdl2:rect-height ,rect) ,h)
     ,@body)))

@export
(defmacro with-rect-from-rect ((rect from-rect) &body body)
  "Helper to use an existing rect for the initial data load"
  (let ((%from-rect (gensym)))
    `(let ((,%from-rect ,from-rect))
       (with-rect (,rect (sdl2:rect-x ,%from-rect) (sdl2:rect-y ,%from-rect) (sdl2:rect-width ,%from-rect) (sdl2:rect-height ,%from-rect))
       ,@body))))

@export
(defmacro with-inflated-rect ((inflated-rect from-rect width-change height-change) &body body)
  "Helper to inflate/deflate a from-rect"
  `(with-rect-from-rect (,inflated-rect ,from-rect)
     (incf (sdl2:rect-width ,inflated-rect) ,width-change)
     (incf (sdl2:rect-height ,inflated-rect) ,height-change)
     ,@body))

@export
(defmacro with-point ((point x y) &body body)
  "Helper macro for a stack point"
  (let ((sz (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-point))))
    `(cffi:with-foreign-pointer (,point ,sz)
       (setf (sdl2:point-x ,point) ,x)
       (setf (sdl2:point-y ,point) ,y)
       ,@body)))

@export
(defun rect-string (rect)
  (format nil "(~a ~a ~a ~a)"
          (sdl2:rect-x rect)
          (sdl2:rect-y rect)
          (sdl2:rect-width rect)
          (sdl2:rect-height rect)))

@export
(defun move-rect (rect x y)
  "Destructively updates the given rect to move its x and y the specified amount.
   x and y are rounded so that the caller doesn't need to worry about converting fractional moves."
  (incf (sdl2:rect-x rect) (round x))
  (incf (sdl2:rect-y rect) (round y))
  rect)

@export
(defun set-rect (rect &key x y w h)
  "Update any/all of the possible rect properties.
   The standard x, y, w (width), and h (height) correspond directly to the underlying SDL_Rect."
  (when x (setf (sdl2:rect-x rect) x))
  (when y (setf (sdl2:rect-y rect) y))
  (when w (setf (sdl2:rect-width rect) w))
  (when h (setf (sdl2:rect-height rect) h)))

@export
(defun rect-dims (rect)
  (values
    (sdl2:rect-x rect)
    (sdl2:rect-y rect)
    (sdl2:rect-width rect)
    (sdl2:rect-height rect)
    rect))

(deftype rect-dim ()
  '(member :top :left :bottom :right
           :topleft :bottomleft :topright :bottomright
           :midtop :midleft :midbottom :midright
           :center :centerx :centery))
@export
(defun rect-dim (rect dim)
  "Query a rect dim with an intuitive keyword name,
   defined by the rect-dim type. If a name gives
   both an x and a y part, a list of (x y) will be returned."
  (declare (type rect-dim dim))
  (multiple-value-bind (x y w h) (rect-dims rect)
    (ecase dim
      (:top y)
      (:left x)
      (:bottom (+ y h))
      (:right (+ x w))
      (:topleft (list x y))
      (:bottomleft nil)
      (:topright nil)
      (:bottomright nil)
      (:midtop nil)
      (:midleft nil)
      (:midbottom nil)
      (:midright nil)
      (:center nil)
      (:centerx (truncate (+ x (/ w 2))))
      (:centery (truncate (+ y (/ h 2))))
      )))

@export
(defun (setf rect-dim) (value rect dim)
  "An extended form of set-rect, allows updating the underlying rect
   with intuitive keyword names defined by the rect-dim type.
   When both an x and a y can be specified, as in :topleft, a sequence of
   two integers in x,y order is expected."
  (declare (type rect-dim dim))
  (multiple-value-bind (x y w h) (rect-dims rect)
    (ecase dim
      (:top (setf (sdl2:rect-y rect) value))
      (:left (setf (sdl2:rect-x rect) value))
      (:bottom (setf (sdl2:rect-y rect) (- value h)))
      (:right (setf (sdl2:rect-x rect) (- value w)))
      (:topleft (setf (sdl2:rect-x rect) (elt value 0))
       (setf (sdl2:rect-y rect) (elt value 1)))
      (:bottomleft nil)
      (:topright nil)
      (:bottomright nil)
      (:midtop nil)
      (:midleft nil)
      (:midbottom nil)
      (:midright nil)
      (:center nil)
      (:centerx nil)
      (:centery nil)
    )))

@export
(defun get-texture-rect (texture)
  (sdl2:make-rect 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture)))

@export
(defun collide-rect? (rect1 rect2)
  (multiple-value-bind (x1 y1 w1 h1) (rect-dims rect1)
    (multiple-value-bind (x2 y2 w2 h2) (rect-dims rect2)
      (and
        (or (and (>= x1 x2) (< x1 (+ x2 w2)))
            (and (>= x2 x1) (< x2 (+ x1 w1)))) ; r1 left edge inside r2, or r2 left edge inside r1
        (or (and (>= y1 y2) (< y1 (+ y2 h2)))
            (and (>= y2 y1) (< y2 (+ y1 h1)))))))) ; r1 bottom edge inside r2, or r2 bottom edge inside r1

@export
(defun outside-screen? (rect)
  (not (collide-rect? lgame.state:*screen-rect* rect)))

@export
(defun contains? (rect1 rect2)
  "Does rect1 completely contain rect2?
   If the rects share edges, that counts as containing."
  (multiple-value-bind (x1 y1 w1 h1) (rect-dims rect1)
    (multiple-value-bind (x2 y2 w2 h2) (rect-dims rect2)
      (and
        (<= x1 x2) (> (+ x1 w1) x2) (>= (+ x1 w1) (+ x2 w2))
        (<= y1 y2) (> (+ y1 h1) y2) (>= (+ y1 h1) (+ y2 h2))))))

