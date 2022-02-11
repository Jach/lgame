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
