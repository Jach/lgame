(in-package #:lgame.pathfinding)

(defclass A* ()
  ((size :accessor .size :initarg :size :type sequence :documentation "The size of the search grid, given as a (rows columns) pair")
   (start-pos :accessor .start-pos :initarg :start-pos :type sequence :documentation "A starting point, given as a (row column) pair")
   (end-pos :accessor .end-pos :initarg :end-pos :type sequence :documentation "The end or goal point, given as a (row column) pair")
   (neighbor-fn :accessor .neighbor-fn :initarg :neighbor-fn :type function :documentation "A function taking (row column) pair and returning a list of neighbors with the associated numerical costs to move to the neighbor, i.e. (((neighbor1-row neighbor2-col) cost) ...).")
   (heuristic :accessor .heuristic :initarg :heuristic :initform :euclidean :type (member :euclidean) :documentation "Specify which supported distance heuristic to use")
   (heuristic-weight :accessor .heuristic-weight :initarg :heuristic-weight :initform 1.0 :type single-float :documentation "Specify a weight to apply to heuristic calculations. 1 is standard A*, 0 turns A* into djikstra, higher values turn it into a greedy search.")

   (waypoint-list :accessor .waypoint-list :initform (list) :type list :documentation "A list of (row column) locations, from start-pos to end-pos inclusive, representing the shortest path of node locations to travel to in order to reach the end-pos.")

   (%open-list :accessor .open-list :type pileup:heap)
   (%visited-list :accessor .visited-list)
   (%parent-list :accessor .parent-list))
  (:documentation
    "A* Pathfinding on a grid, or something reasonably resembling a 2D search space.
     :start-pos defines the starting point of the search,
     :end-pos defines the end or goal point. These are expected in (row column) format.
     "))

(defclass a-star (A*)
  ()
  (:documentation "Alias for class A*"))


(defmethod initialize-instance :after ((self A*) &key)
  (setf (.open-list self) (pileup:make-heap #'node-compare :size (apply #'* (.size self))))
  (setf (.visited-list self) (make-array (.size self) :element-type 'boolean)) ; could also be a hash table, maybe experiment? or a morton-number encoded bitstring?
  (setf (.parent-list self) (make-array (.size self) :element-type 'parent-track)))


;; A node along the shortest path,
;; at location (row column).
;; The real-cost represents the raw cost
;; to move from the starting location to this node.
;; The total-cost represents the raw cost
;; plus the addition of an heuristic cost.
;; The parent-r and parent-c represent the (row column)
;; of a neighboring node, the parent, from which
;; the costs of this node have been calculated assuming
;; the parent is along the path just before this node.
(defstruct path-node
  r
  c
  real-cost
  total-cost
  parent-r
  parent-c)

;; I may be able to factor this into the path-node?
;; But it basically helps keep track of the best costs,
;; it's pointed to by the path-node...
;; It's basically done as a pruning step when considering
;; neighbors to add to the open-list. A particular neighbor
;; may be reachable by multiple parents, there's no point
;; in using a parent with a higher real-cost.
;; (Check this... I'm sleepy.)
(defstruct parent-track
  r
  c
  real-cost)

(defun node-compare (node1 node2)
  "Compare two path nodes for the open-list
   priority queue."
  (declare (type path-node node1)
           (type path-node node2))
  (< (path-node-total-cost node1) (path-node-total-cost node2)))


(defun euclidean (a1 b1 a2 b2)
  "Euclidean distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (let ((dist1 (- a1 a2))
        (dist2 (- b1 b2)))
    (sqrt (+ (* dist1 dist1) (* dist2 dist2)))))

(defconstant +sqrt2-minus-1+ (1- (sqrt 2)))

(defun octile (a1 b1 a2 b2)
  "Octile distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (let ((dist1 (abs (- a1 a2)))
        (dist2 (abs (- b1 b2))))
    (+ (* (min dist1 dist2) +sqrt2-minus-1+)
       (max dist1 dist2))))

(defun chebyshev (a1 b1 a2 b2)
  "Chebyshev distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (max (abs (- a1 a2)) (abs (- b1 b2))))

(defun manhattan (a1 b1 a2 b2)
  "Manhattan distance formula for a pair of points
   (a1, b1) and (a2, b2)."
  (+ (abs (- a1 a2) (abs (- b1 b2)))))


(defun calc-cost-and-push (self r c real-cost best-r best-c)
  (let ((heuristic-cost (euclidean (elt (.end-pos self) 0) (elt (.end-pos self) 1) r c)))
    (setf heuristic-cost (* heuristic-cost (.heuristic-weight self)))
    (let ((node (make-path-node :r r :c c :real-cost real-cost :total-cost (+ heuristic-cost real-cost) :parent-r best-r :parent-c best-c)))
      (pileup:heap-insert node (.open-list self))
      (setf (aref (.visited-list self) r c) T)
      (let ((parent (aref (.parent-list self) r c)))
        (setf (parent-track-r parent) best-r
              (parent-track-c parent) best-c
              (parent-track-real-cost parent) real-cost))))
  ; optional debugging, set r,c on the terrain to visited
  )

(defmethod compute-path ((self A*) &key single-step? new-request?)
  ; later, take explicit row, col args that represent *goal-pos*

  (when new-request? ; push start first time this is called
    ; make sure open-list is empty
    (loop until (pileup:heap-empty-p (.open-list self)) do
          (pileup:heap-pop (.open-list self)))
    ; make sure visited-list is set to nil, parent-list structures are clear
    (loop for row below (elt (.size self) 0) do
          (loop for col below (elt (.size self) 1) do
                (setf (aref (.visited-list self) row col) nil)
                (setf (aref (.parent-list self) row col) (make-parent-track :real-cost most-positive-single-float))
                ))
    ; put the starting node location on top of the open-list
    (calc-cost-and-push self (elt (.start-pos self) 0) (elt (.start-pos self) 1) 0.0 (elt (.start-pos self) 0) (elt (.start-pos self) 1)))

  (loop until (pileup:heap-empty-p (.open-list self)) do
        (let ((best-node (pileup:heap-pop (.open-list self))))

          (when (and (= (path-node-r best-node) (elt (.end-pos self) 0))
                     (= (path-node-c best-node) (elt (.end-pos self) 1))) ; goal!
            ; construct the waypoint list
            (setf (.waypoint-list self) (list))
            (push (.end-pos self) (.waypoint-list self))
            ; walk the parent list backwards until we hit the start/cur pos
            ; original also has parent2, sets parent2 to the next one, breaks loop if parent and parent2 are equal. Needed?
            (let ((parent (aref (.parent-list self) (elt (.end-pos self) 0) (elt (.end-pos self) 1))))
              (loop until (and (= (elt (.start-pos self) 0) (parent-track-r parent))
                               (= (elt (.start-pos self) 1) (parent-track-c parent))) do
                    (push (list (parent-track-r parent) (parent-track-c parent)) (.waypoint-list self))
                    (setf parent (aref (.parent-list self) (parent-track-r parent) (parent-track-c parent)))))
            ; push current loc on again for the final (needed?)
            (push (.start-pos self) (.waypoint-list self))

            ; if rubber banding, if spline, do those
            (return-from compute-path T))

          ; debug, set color of best r c to yellow

          ; push neighbors of best-node onto the open-list, subject to these constraints:
          ; 1. Up to 8 neighbors since we're on a grid
          ; 2. A movement to the neighbor is valid
          ; 3. The neighbor hasn't been visited before
          ; 4. The cost to visit the neighbor is less than the real cost of the parent list for that cell found via any other path

          ; #2 is application dependent. A particular node may represent a whole object like empty-spot or solid-wall or traversible-but-high-cost-sandpit,
          ; or it may represent a spot with neighbor info contained inside... for this maze, only orthogonal neighbors can be visited,
          ; and only if there's no wall, this is annoying to determine. need a generic solution here...
          ; basically having a 'terrain' from which one gets coords (which may map 1-1 with the a* row-col, as here, or not)
          ; and can query 'terrain' for neighbors
          ; original has ISWALL which queries terrain for is-a-wall, but yeah, really just want get-neighbors on terrrain
          ; original also uses fancy bitwise algo to do it, not gonna do that here

          (let ((neighbors-with-cost (funcall (.neighbor-fn self) (list (path-node-r best-node) (path-node-c best-node))))
                ; if analysis, can also query terrain for 'influence' value of the best node, times 20 for some reason, which can be added to the cost
                (initial-cost (path-node-real-cost best-node)))
            (dolist (neighbor-cost neighbors-with-cost)
              (let* ((neighbor (elt neighbor-cost 0))
                     (cost (elt neighbor-cost 1))
                     (cost-adj (+ initial-cost cost)) ; + influence if analysis..
                     (neighbor-row (elt neighbor 0))
                     (neighbor-col (elt neighbor 1)))
                (when (and (not (aref (.visited-list self) neighbor-row neighbor-col))
                           (< cost-adj (parent-track-real-cost (aref (.parent-list self) neighbor-row neighbor-col))))
                  (calc-cost-and-push self neighbor-row neighbor-col cost-adj (path-node-r best-node) (path-node-c best-node)))))))

        (unless single-step? ; we'll be called again later
          (return-from compute-path nil)))
  ; we reached the end of the loop without finding a path or returning early,
  ; flag the search as failed
  (values nil nil))
