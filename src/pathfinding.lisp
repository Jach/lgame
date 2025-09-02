(in-package #:lgame.pathfinding)

(defclass pathfinding ()
  ((size :accessor .size
         :initarg :size
         :type sequence
         :documentation "The size of the search grid, given as a (rows columns) pair")
   (start-pos :accessor .start-pos
              :initarg :start-pos
              :type sequence
              :documentation "A starting position, given as a (row column) pair")
   (end-pos :accessor .end-pos
            :initarg :end-pos
            :type sequence
            :documentation "The end or goal position, given as a (row column) pair")

   (waypoint-list :accessor .waypoint-list
                  :initform (list)
                  :type list
                  :documentation "A list of (row column) locations, from start-pos to end-pos inclusive,
                                  representing the shortest path of node locations to travel to in order to reach the end-pos.
                                  When COMPUTE-PATH returns true, this list will be non-empty to access the path found if it exists.
                                  It will remain nil when no path can or has yet been found."
                  ))
  (:documentation "The primary common structure of any pathfinding algorithm with the first three initargs always expected as input
                   and a waypoint list for the path as the output."))

(defmethod shortest-path ((self pathfinding))
  "Returns the shortest path as a list of ( (row column) ...) locations,
   with the first item being equivalent to the starting position, and the final item being equivalent to the ending position.
   If there is no path known or computed yet, this will return nil."
  (.waypoint-list self))

(defclass A* (pathfinding)
  ((neighbor-fn :accessor .neighbor-fn
                :initarg :neighbor-fn
                :initform (lambda (row col) (declare (ignorable row col)))
                :type function
                :documentation "A function taking a (row column) pair representing a source location, and returning a sequence of neighbor locations
                                with the associated numerical costs to move from the source location to the neighbor.
                                i.e. ( ((neighbor1-row neighbor2-col) cost)
                                      ((neighbor2-row neighbor2-col) cost)
                                      ...).
                                For 8-directional movement on an empty grid, costs to the orthogonally adjacent neighbors would typically be 1,
                                and costs to diagonal neighbors would typically be (sqrt 2) ~= 1.41.
                                A neighboring spot might not necessarily be open but could be e.g. a sand trap and thus have a higher cost, like 5.
                                A neighboring spot that is inaccessible (such as a wall) should just not be part of the returned neighbors at all.
                                This function is called during COMPUTE-PATH pathfinding to expand the open set of locations to check.")
   (heuristic :accessor .heuristic
              :initarg :heuristic
              :initform :euclidean
              :type (member :euclidean :octile :chebyshev :manhattan :zero)

              :documentation "Specify which supported distance heuristic to use, including a :zero metric which effectively turns A* into djikstra's algorithm.")

   (%heuristic-fn :accessor .heuristic-fn
                  :initform #'euclidean
                  :documentation "Internal heuristic function that is called, set after construction and selection of an heuristic.")
   (heuristic-weight :accessor .heuristic-weight
                     :initarg :heuristic-weight
                     :initform 1.0
                     :type single-float
                     :documentation "Specify a weight to apply to heuristic calculations. 1 is standard A*, 0 turns A* into djikstra regardless of heuristic,
                                     and higher values turn it more into a greedy best-first search.")

   (%open-nodes :accessor .open-nodes
               :type lgame.data-structures:priority-queue
               :documentation "Represents the collection of locations to explore in the search,
                               with the one returned by methods POP or TOP being the next priority candidate.

                               Each element is an OPEN-PATH-NODE structure which contains the node's own location (row column),
                               the 'real cost' (g-cost in some literature) representing the actual cumulative cost from the start location to this node's location,
                               and the 'total estimated cost' (f-cost in some literature) which represents the real cost + estimated heuristic cost (h-cost) of this node to the goal.

                               Priority is determined by having the least total estimated cost.

                               If this collection is empty, that means the algorithm has run out of potential locations to
                               search for a path to the goal, and thus there is no path.")
   (%seen-nodes :accessor .seen-nodes
                  :type array
                  :documentation "A multi-dimensional array of the same size as the given .size.

                                  Each element is either nil or a SEEN-PATH-NODE structure at location [row][col]
                                  indicating that said location (row col) has been or is still on the open-nodes.
                                  Notably the element stores (parent-row parent-col) as the location of the parent node from which
                                  it was put on the open-nodes, and the cumulative best-real-cost (known so far anyway) of the path from the start, to the parent, to this node.

                                  If a path to the goal is found, the full path is easily reconstructed by walking from
                                  seen[goal-row][goal-col] -> the stored parent row/col to seen[parent-row][parent-col] -> all the way back until reaching the starting position.
                                  from that parent."))
  (:documentation
    "A* Pathfinding on a grid, or something reasonably resembling a 2D search space.
     :start-pos defines the starting point of the search,
     :end-pos defines the end or goal point. These are expected in (row column) format."))

(defclass a-star (A*)
  ()
  (:documentation "Alias for class A*"))


(defmethod initialize-instance :after ((self A*) &key (backing-collection :unsorted-vector))
  (setf (.heuristic-fn self) (case (.heuristic self)
                               (:euclidean #'euclidean)
                               (:manhattan #'manhattan)
                               (:octile #'octile)
                               (:chebyshev #'chebyshev)
                               (:zero #'zero)))
  (setf (.open-nodes self) (make-instance 'lgame.data-structures:priority-queue
                                          :backing-collection backing-collection
                                          :size (apply #'* (.size self))
                                          :comparison-fn #'node-compare))
  (setf (.seen-nodes self) (make-array (.size self) :initial-element nil)))


(defstruct open-path-node
  "A node in the graph put on the open nodes collection with data about the path from the starting location to this node.
   The ROW and COL attributes are the location of this node itself.
   The REAL-COST, also known as the g-cost, is the cumulative cost of the path from the starting location to this node.
   The TOTAL-ESTIMATED-COST, also known as the f-cost, is the REAL-COST + estimated heuristic-cost (h-cost), where the h-cost represents a guess at the cost from this node to the goal node.
   "
  (row 0 :type fixnum :read-only t)
  (col 0 :type fixnum :read-only t)
  (real-cost 0.0d0 :type double-float :read-only t)
  (total-estimated-cost 0.0d0 :type double-float :read-only t))

(defstruct seen-path-node
  "A node in the graph put on the seen nodes array. This node's location is implicit in the seen-nodes[row][column] access.
   PARENT-ROW and PARENT-COL are the location of the parent node from which a path to this node comes from.
   BEST-REAL-COST is the cumulative cost (g-cost) of the path from the starting node, through the parent node, and to this node, known so far.
   The values of these attributes may be changed if another path to this node from a different parent with a lower cost to this node is found."
  (parent-row 0 :type fixnum)
  (parent-col 0 :type fixnum)
  (best-real-cost 0.0d0 :type double-float))

(declaim (ftype (function (open-path-node open-path-node) boolean) node-compare))
(defun node-compare (node1 node2)
  "Compare two path nodes for the open-nodes priority queue."
  (< (open-path-node-total-estimated-cost node1) (open-path-node-total-estimated-cost node2)))

(declaim (inline start-row start-col goal-row goal-col))

(declaim (ftype (function (pathfinding) fixnum) start-row))
(defun start-row (pathfinder)
  (elt (.start-pos pathfinder) 0))

(declaim (ftype (function (pathfinding) fixnum) start-col))
(defun start-col (pathfinder)
  (elt (.start-pos pathfinder) 1))

(declaim (ftype (function (pathfinding) fixnum) goal-row))
(defun goal-row (pathfinder)
  (elt (.end-pos pathfinder) 0))

(declaim (ftype (function (pathfinding) fixnum) goal-col))
(defun goal-col (pathfinder)
  (elt (.end-pos pathfinder) 1))

(declaim (inline elapsed))
(defun elapsed (start-time)
  (- (lgame.time:now-seconds) start-time))

(defmethod compute-path ((self A*) &key (compute-in-single-step? t) (new-request? t) &aux start-secs)
  "Computes the shortest path to the goal node. Returns multiple values, described below.

   If COMPUTE-IN-SINGLE-STEP? this function tries to find the path in one go.
   If false, then it will only do enough work to advance the algorithm's search by one node at a time.

   If NEW-REQUEST? this resets any progress or found path from before and starts over from the starting position.
   If false, then it will continue progress towards finding the path or return immediately if a path was previously found.

   Thus if performing A* search incrementally, the first call should have :compute-in-single-step nil and :new-request? t,
   and subsequent calls should have :compute-in-single-step? nil and :new-request? nil.

   The first returned value is either T or nil. With the default keyword arguments, it indicates whether a path was found or not.
   When COMPUTE-IN-SINGLE-STEP? is nil, then T still indicates a path was found, but nil indicates that the third value
   must be observed to learn whether work remains or no path can be found.

   The second value is always the measured execution time of the function call in floating point seconds.

   The third value is always nil in the case of the default keyword arguments. When COMPUTE-IN-SINGLE-STEP? is nil, then this third value is T if there is more work to be done,
   or nil if no more work can be done. (Either the path was found or not findable according to the first value.)

   When a path is found, SHORTEST-PATH can be used to retrieve it."
  (setf start-secs (lgame.time:now-seconds))
  (when new-request?
    (cleanup-for-new-request self))

  (loop until (lgame.data-structures:priority-queue-empty? (.open-nodes self)) do
        (let ((best-node (lgame.data-structures:priority-queue-pop (.open-nodes self))))
          (when (reached-goal? self best-node)
            (construct-waypoint-list self)
            (return-from compute-path (values T (elapsed start-secs) nil)))

          (get-and-push-neighbors self best-node)
          (unless compute-in-single-step? ; return early, we'll be called again later
            (return-from compute-path (values nil (elapsed start-secs) T )))))
  ; we reached the end of the loop without finding a path or returning early, so no path

  (values nil (elapsed start-secs) nil))

(defun get-and-push-neighbors (self best-node)
  (let ((neighbors-with-cost (funcall (.neighbor-fn self) (list (open-path-node-row best-node) (open-path-node-col best-node))))
        (initial-cost (open-path-node-real-cost best-node)))
    (dolist (neighbor-cost neighbors-with-cost)
      (let* ((neighbor (elt neighbor-cost 0))
             (cost-adj-from-best-to-neighbor (elt neighbor-cost 1))
             (real-cost (+ initial-cost cost-adj-from-best-to-neighbor)) ; + influence if analysis..
             (neighbor-row (elt neighbor 0))
             (neighbor-col (elt neighbor 1)))
        (calc-heuristic-and-push-to-open-nodes self
                                               neighbor-row neighbor-col
                                               real-cost
                                               best-node)))))

(defun cleanup-for-new-request (self)
  ;; make sure seen-nodes is clear
  (loop for row below (elt (.size self) 0) do
        (loop for col below (elt (.size self) 1) do
              (setf (aref (.seen-nodes self) row col) nil)))

  ;; make sure open-nodes is empty
  (loop until (lgame.data-structures:priority-queue-empty? (.open-nodes self)) do
        (lgame.data-structures:priority-queue-pop (.open-nodes self)))
  ;; then push the starting location onto it, with a 0 cost and itself as a parent,
  ;; which will also mark it in the seen nodes
  (calc-heuristic-and-push-to-open-nodes self (start-row self) (start-col self) 0.0d0
                                         (make-open-path-node :row (start-row self) :col (start-col self)))

  ;; erase any previous waypoint list
  (setf (.waypoint-list self) (list)))

(defun reached-goal? (self best-node)
  (and (= (open-path-node-row best-node) (goal-row self))
       (= (open-path-node-col best-node) (goal-col self))))

(defmethod construct-waypoint-list ((self A*))
  (setf (.waypoint-list self) (list))
  (push (.end-pos self) (.waypoint-list self))
  ; walk the parents of seen-nodes[goal] backwards until we hit the start pos
  (let ((parent (aref (.seen-nodes self) (goal-row self) (goal-col self))))
    (loop until (and (= (start-row self) (seen-path-node-parent-row parent))
                     (= (start-col self) (seen-path-node-parent-col parent)))
          do
          (push (list (seen-path-node-parent-row parent) (seen-path-node-parent-col parent))
                (.waypoint-list self))
          (setf parent (aref (.seen-nodes self) (seen-path-node-parent-row parent) (seen-path-node-parent-col parent)))))
  ; push starting pos on at last
  (push (.start-pos self) (.waypoint-list self)))

(defun calc-heuristic-and-push-to-open-nodes (self new-path-row new-path-col
                                                   new-real-cost ; from best node, to path node
                                                   best-parent-node)
  "Pushes the path-node-to-push to the open-nodes collection along with its full estimated cost that includes the heuristic cost,
   so long as it hasn't been pushed before, or so long as the new-real-cost of this path is lower than an existing cost.
   Updates the seen nodes to include it as well."
  (let* ((best-parent-row (open-path-node-row best-parent-node))
         (best-parent-col (open-path-node-col best-parent-node))
         (seen-info (aref (.seen-nodes self) new-path-row new-path-col)))
    ; if we have seen the new path row,col before and our new cost is >= the already seen cost, bail early as this new one isn't better
    (when (and seen-info
               (>= new-real-cost (seen-path-node-best-real-cost seen-info)))
      (return-from calc-heuristic-and-push-to-open-nodes nil))

    (let ((heuristic-cost (* (.heuristic-weight self)
                             (funcall (.heuristic-fn self) new-path-row new-path-col (goal-row self) (goal-col self)))))
      (lgame.data-structures:priority-queue-push (.open-nodes self) (make-open-path-node
                                                                      :row new-path-row :col new-path-col
                                                                      :real-cost new-real-cost
                                                                      :total-estimated-cost (+ heuristic-cost new-real-cost))))

    (setf (aref (.seen-nodes self) new-path-row new-path-col) (make-seen-path-node
                                                                        :parent-row best-parent-row :parent-col best-parent-col
                                                                        :best-real-cost new-real-cost))))


(defclass floyd-warshall (pathfinding)
  ((clear-path?-fn :accessor .clear-path?-fn
                   :initarg :clear-path?-fn
                   :initform (lambda (r0 c0 r1 c1) (declare (ignore r0 c0 r1 c1)))
                   :documentation "A function provided by the map/terrain that takes a starting location at (row0, col0) and an ending location at (row1, col1).
                                   It should return true if there is a clear path (free of impassable obstacles) from (row0, col0) to (row1, col1)."
                   )

   (%dist :accessor .dist
          :documentation "Array of distance calculations, stored for multiple calls to COMPUTE-PATH with changing start/end positions.")
   (%parent :accessor .parent
            :documentation "Array of parent indices."))
  (:documentation "Floyd-Warshall is a pathfinding algorithm that computes the transitive closure of a graph, which means it computes the shortest
                   paths from every node to every other node. It is an expensive calculation but if a map is static it beats redoing A* a bunch."))

(defmethod compute-path ((self floyd-warshall) &key (compute-in-single-step? t) (new-request? t) &aux start-secs)
  "Computes the Floyd-Warshall matrix if NEW-REQUEST?. A new request is not needed if the goal changes, only if the map changes.
   Returns multiple values.
   Warning, the initial computation is expensive, with O(V^3) where V = map vertices = (* rows cols).
   If a path from the start to the goal position is found, returns T as the first value. The path will be available with SHORTEST-PATH.
   If a path can't be found, returns nil as the first value.
   The second value is always the measured execution time of the function call in floating point seconds.
   The keyword argument compute-in-single-step? is ignored." ; todo, make this an A* property.
  (declare (ignore compute-in-single-step?))
  (declare (optimize (speed 3)))
  ;  (declare (optimize (debug 3)))
  (setf start-secs (lgame.time:now-seconds))
  (let* ((rows (the fixnum (elt (.size self) 0)))
         (cols (the fixnum (elt (.size self) 1)))
         (v (the fixnum (* rows cols)))
         (clear-path? (the function (.clear-path?-fn self))))
    (flet ((idx (r c) (the fixnum (+ c (the fixnum (* cols r))))))
      (when new-request?
        (let ((dist (make-array (list v v) :element-type 'double-float :initial-element most-positive-double-float))
              (parent (make-array (list v v) :element-type 'fixnum :initial-element -1)))
          (setf (.dist self) dist
                (.parent self) parent)
          (loop for r0 below rows do
                (loop for c0 below cols
                      for cell0 = (idx r0 c0) do
                      (loop for r1 below rows do
                            (loop for c1 below cols
                                  for cell1 = (idx r1 c1) do
                                  (setf (aref parent cell0 cell1) cell0)
                                  (cond
                                    ((eql cell0 cell1)
                                     (setf (aref dist cell0 cell1) 0.0d0))
                                    ((funcall clear-path? r0 c0 r1 c1) ; straight-line connection
                                     (setf (aref dist cell0 cell1) (feuclidean r0 c0 r1 c1))))))))
          ;; Core Floyd-Warshall
          (loop for k below v do
                (loop for i below v
                      for dik = (aref dist i k)
                      unless (eql dik most-positive-double-float) do
                      (loop for j below v
                            for kj = (aref dist k j)
                            for dij = (aref dist i j)
                            unless (eql kj most-positive-double-float) do
                            (let ((sum (+ dik kj)))
                              (when (< sum dij)
                                (setf (aref dist i j) sum
                                      (aref parent i j) (aref parent k j)))))))))

      (let ((start (idx (start-row self) (start-col self)))
            (goal (idx (goal-row self) (goal-col self))))
        (if (eql most-positive-fixnum (aref (.dist self) start goal))
          (values nil (elapsed start-secs))
          (progn (construct-waypoint-list self)
                 (values T (elapsed start-secs))))))))


(defmethod construct-waypoint-list ((self floyd-warshall))
  "For Floyd-Warshall, the shortest path is also automatically pruned to skip any intermediate points.
   That is, if there's a clear path from A to C, there's no need to stop at intermediary points B.
   Thus consider linearly interpolating the path if you don't want the appearance of 'teleporting'."
  (let* ((cols (elt (.size self) 1)))
    (flet ((idx (r c) (+ c (* cols r)))
           (row-of (idx) (floor idx cols))
           (col-of (idx) (mod idx cols)))
      (let* ((start-r (start-row self))
             (start-c (start-col self))
             (goal-r (goal-row self))
             (goal-c (goal-col self))
             (start (idx start-r start-c))
             (goal (idx goal-r goal-c)))
        (setf (.waypoint-list self) (list))
        (loop until (eql start goal) do
          (push (list goal-r goal-c) (.waypoint-list self))
          (setf goal (aref (.parent self) start goal)
                goal-r (row-of goal)
                goal-c (col-of goal)))
        (push (list start-r start-c) (.waypoint-list self))))))
