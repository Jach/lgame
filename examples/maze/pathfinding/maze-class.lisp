#|
The actual maze structure, independent of the GUI
|#
(in-package #:lgame.example.maze-pathfinding)

(defclass maze ()
  ((dimensions :accessor .dims :initarg :dimensions :documentation "Two-element list of the maze's dimensions, format (rows columns)")
   (maze :accessor .maze)
   (random-state :initform (make-random-state t))))

(defstruct node
  south-wall
  east-wall
  visited?)

(defmethod initialize-instance :after ((self maze) &key)
  (setf (.maze self) (make-array (.dims self) :element-type 'node))
  (loop for row below (first (.dims self)) do
        (loop for col below (second (.dims self)) do
              (setf (aref (.maze self) row col) (make-node :south-wall t :east-wall t :visited? nil)))))

(defmethod print-object ((self maze) stream)
  (let ((rows (first (.dims self)))
        (cols (second (.dims self))))
    (dotimes (i cols) (write-string " _" stream))
    (terpri stream)
    (loop for y below rows do
          (write-string "|" stream)
          (loop for x below cols do
                (if (node-south-wall (aref (.maze self) y x))
                    (write-string "_" stream)
                    (write-string " " stream))
                (if (node-east-wall (aref (.maze self) y x))
                    (write-string "|" stream)
                    (write-string " " stream)))
          (terpri stream))))

(defmethod generate ((self maze))
  "Generates the maze, accessible by .maze"
  (let (;(start-cell (list 0 0))
        (start-cell (list (1- (first (.dims self)))
                          (1- (second (.dims self)))))
        )
    ; assuming you start the maze at the top left and want to reach the bottom right,
    ; it's subjectively more difficult if the generation began at the bottom right.
    (let ((*random-state* (slot-value self 'random-state)))
      (generate-helper (.maze self) (list start-cell)))))

(defun generate-helper (maze stack)
  "Generates a perfect maze (every part of the maze is reachable from any other part)
   using a simple recursive algorithm.
   Maze is expected to be a 2D array of nodes.

   Popping a (row col) pair from the stack, we visit the node.
   If it hasn't previously been visited, we get its neighbors,
   shuffle them, and for each one we attempt to knock down the
   wall between the node and the neighbor, then recurse into
   the neighbor.
   Eventually when the stack is empty all cells
   will have been visited and we are done."
  (when (null stack)
    (return-from generate-helper))
  (let* ((cur-cell-pos (pop stack))
         (neighbors (get-neighbors maze cur-cell-pos)))
    (setf neighbors (alexandria:shuffle neighbors))
    (dolist (neighbor-pos neighbors)
      (let ((neighbor (aref maze (first neighbor-pos) (second neighbor-pos))))
        (unless (node-visited? neighbor)
          (setf (node-visited? neighbor) t)
          (push neighbor-pos stack)
          (knock-wall maze cur-cell-pos neighbor-pos)
          (generate-helper maze stack))))))

(defun get-neighbors (maze cell)
  "Return list of pairs of neighbor coordinates of the cell coordinates, bound by the maze dimensions,
   all coordinates in (row col) form, aka (y x)."
  (let* ((dims (array-dimensions maze))
         (r (first cell))
         (c (second cell))
         (west (list r (1- c)))
         (east (list r (1+ c)))
         (north (list (1- r) c))  ; origin 0,0 starts at top-left, so decrementing column is how to go 'north'
         (south (list (1+ r) c)))
    (remove-if-not (lambda (el) (within-bounds? el dims))
                   (list north south east west))))

(defun within-bounds? (cell bounds)
  "T if cell is contained in bounds,
   with 0 being an implicit lower bound."
  (let ((r1 (first cell))
        (c1 (second cell))
        (r2 (first bounds))
        (c2 (second bounds)))
    (and (<= 0 r1 (1- r2))
         (<= 0 c1 (1- c2)))))

(defun knock-wall (maze cell neighbor)
  "Knocks down the wall between the cell and its neighbor"
  ; determine which direction neighbor is relative to cell, then knock the wall
  (let ((row-cel (first cell)) ;
        (col-cel (second cell))
        (row-nei (first neighbor))
        (col-nei (second neighbor)))
    (cond
      ((and (= col-cel col-nei) (= row-cel (1+ row-nei))) ; cel row is one above neighbor row, making neighbor 'above'/north, so knock out its south wall
       (setf (node-south-wall (aref maze row-nei col-nei)) nil))
      ((and (= col-cel col-nei) (= row-cel (1- row-nei))) ; neighbor is below, knock out cell's south wall
       (setf (node-south-wall (aref maze row-cel col-cel)) nil))
      ((and (= row-cel row-nei) (= col-cel (1+ col-nei))) ; neighbor is to the west, knock its east wall
       (setf (node-east-wall (aref maze row-nei col-nei)) nil))
      ((and (= row-cel row-nei) (= col-cel (1- col-nei))) ; neighbor is to the east, knock cell's east wall
       (setf (node-east-wall (aref maze row-cel col-cel)) nil)))))

(defun finished-valid-neighbors (maze cell)
  "Once the maze has been constructed, this will return the same thing as get-neighbors, but with the addition
   that if there are any walls between the cell and a neighbor, that neighbor will not be returned.
   This is useful for path-finding.
   Additionally, the cost of moving to each neighbor is included (1),
   giving a structure of ((neighbor cost) ...)"
  (let ((row-cel (first cell))
        (col-cel (second cell))
        (neighbors (get-neighbors maze cell)))
    (mapcar (lambda (neighbor)
              (vector neighbor 1))
            (remove-if (lambda (neighbor)
                         (let ((row-nei (first neighbor))
                               (col-nei (second neighbor)))
                           (cond
                             ((and (= col-cel col-nei) (= row-cel (1+ row-nei))) ; cel row is one above neighbor row, making neighbor 'above'/north, so check if there is a south wall on neighbor
                              (node-south-wall (aref maze row-nei col-nei)))
                             ((and (= col-cel col-nei) (= row-cel (1- row-nei))) ; neighbor is below, check for south wall on cell
                              (node-south-wall (aref maze row-cel col-cel)))
                             ((and (= row-cel row-nei) (= col-cel (1+ col-nei))) ; neighbor is to the west, check its east wall
                              (node-east-wall (aref maze row-nei col-nei)))
                             ((and (= row-cel row-nei) (= col-cel (1- col-nei))) ; neighbor is to the east, check cell's east wall
                              (node-east-wall (aref maze row-cel col-cel))))))
                       neighbors))))


;;;; Below used for Floyd-Warshall
;;;; It's not quite correct I think when it comes to diagonal travel until I expanded the puff size to 0.51 (0.05 should be more normal?)
;;;; but seems fine enough now, just going to leave it. It works!
;;;; Takes about 4-7 seconds on my machine to generate the FW matrix on the biggest map...but then waypoint lists are ~free.

(defun clear-path? (maze r0 c0 r1 c1 &key (puff 0.51))
  "Return T if the straight path from (r0,c0) to (r1,c1) is clear of walls.
   Puff adds tolerance around walls."
  ;; The general strategy here is to create a bounding box around the start and end locations,
  ;; say each cell has unit width/height and construct a line segment from the middle of
  ;; the start cell to the middle of the end cell.
  ;; Go through each cell in the bounding box, and if there is a wall, test for whether
  ;; the line segment intersects with that wall or not.
  (let* ((x0 (+ c0 0.5))
         (y0 (+ r0 0.5))
         (x1 (+ c1 0.5))
         (y1 (+ r1 0.5))
         (min-row (min r0 r1))
         (max-row (max r0 r1))
         (min-col (min c0 c1))
         (max-col (max c0 c1)))
    (loop for row from min-row to max-row do
          (loop for col from min-col to max-col
                for node = (aref maze row col)
                do
                ;; South wall of this cell
                (when (node-south-wall node)
                  (let ((wx0 (- col puff))
                        (wy0 (+ row 1))
                        (wx1 (+ col 1 puff))
                        (wy1 (+ row 1)))
                    (when (segments-intersect? x0 y0 x1 y1 wx0 wy0 wx1 wy1)
                      (return-from clear-path? nil))))
                ;; East wall of this cell
                (when (node-east-wall node)
                  (let ((wx0 (+ col 1))
                        (wy0 (- row puff))
                        (wx1 (+ col 1))
                        (wy1 (+ row 1 puff)))
                    (when (segments-intersect? x0 y0 x1 y1 wx0 wy0 wx1 wy1)
                      (return-from clear-path? nil)))))))
  t)

#+nil
(progn
(segments-intersect? 0 0 2 2 0 2 2 0) ; T (cross)
(segments-intersect? 0 0 1 1 2 2 3 3) ; NIL (parallel disjoint)
(segments-intersect? 0 0 2 2 1 1 3 3) ; T (collinear overlap)
(segments-intersect? 0 0 0 2 0 1 0 3) ; T (collinear vertical overlap)
)

(defun orientation (x1 y1 x2 y2 x3 y3)
  "Return the orientation of the triplet (p1, p2, p3).
   >0 = counterclockwise, <0 = clockwise, =0 = collinear."
  (- (* (- y2 y1) (- x3 x2))
     (* (- x2 x1) (- y3 y2))))

(defun on-segment? (x1 y1 x2 y2 x3 y3)
  "Check if (x2, y2) lies on the segment (x1, y1) - (x3, y3)."
  (and (<= (min x1 x3) x2 (max x1 x3))
       (<= (min y1 y3) y2 (max y1 y3))))

(defun segments-intersect? (x1 y1 x2 y2 x3 y3 x4 y4)
  "Return T if segment (x1,y1)-(x2,y2) intersects (x3,y3)-(x4,y4)."
  (let ((o1 (orientation x1 y1 x2 y2 x3 y3))
        (o2 (orientation x1 y1 x2 y2 x4 y4))
        (o3 (orientation x3 y3 x4 y4 x1 y1))
        (o4 (orientation x3 y3 x4 y4 x2 y2)))
    (or (and (< (* o1 o2) 0) (< (* o3 o4) 0)) ; proper intersection
        (and (zerop o1) (on-segment? x1 y1 x3 y3 x2 y2))
        (and (zerop o2) (on-segment? x1 y1 x4 y4 x2 y2))
        (and (zerop o3) (on-segment? x3 y3 x1 y1 x4 y4))
        (and (zerop o4) (on-segment? x3 y3 x2 y2 x4 y4)))))
;(lgame.display:screenshot-png "/tmp/fw.png")
