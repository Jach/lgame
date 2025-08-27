(in-package #:lgame.pathfinding)

(declaim (inline euclidean octile chebyshev manhattan zero))

(declaim (ftype (function (number number number number) number) euclidean))
(declaim (ftype (function (number number number number) number) octile))
(declaim (ftype (function (number number number number) number) chebyshev))
(declaim (ftype (function (number number number number) number) manhattan))
(declaim (ftype (function (number number number number) number) zero))

(defun euclidean (a1 b1 a2 b2)
  "Euclidean distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (let ((dist1 (- a1 a2))
        (dist2 (- b1 b2)))
    (sqrt (+ (* dist1 dist1) (* dist2 dist2)))))

(defun octile (a1 b1 a2 b2)
  "Octile distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (let ((dist1 (abs (- a1 a2)))
        (dist2 (abs (- b1 b2))))
    (+ (* (min dist1 dist2) #.(1- (sqrt 2)))
       (max dist1 dist2))))

(defun chebyshev (a1 b1 a2 b2)
  "Chebyshev distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (max (abs (- a1 a2)) (abs (- b1 b2))))

(defun manhattan (a1 b1 a2 b2)
  "Manhattan distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (+ (abs (- a1 a2)) (abs (- b1 b2))))

(defun zero (a1 b1 a2 b2)
  "Same as no heuristic, reduces A* to Djikstra"
  (declare (ignore a1 b1 a2 b2))
  0.0)
