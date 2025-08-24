(in-package #:lgame.example.maze-pathfinding)

(defparameter *maps*
  '(
    :map1
" 0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0 -1  0
 0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0
-1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0
 0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1
 0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0 -1  0  0  0
 0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0
 0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0
-1 -1  0  0 -1  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0
 0  0  0 -1  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0
 0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1
 0 -1 -1 -1  0 -1  0 -1  0  0 -1  0 -1  0  0  0  0 -1  0  0
 0  0  0 -1  0  0 -1  0  0 -1  0  0  0 -1  0 -1  0 -1  0  0
 0  0  0 -1  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0
-1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0  0 -1
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0 -1 -1 -1 -1 -1 -1 -1 -1  0  0  0  0  0  0 -1  0 -1  0  0
 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0"
    :map2
"0  0  0  0  0 -1  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0
 0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0
-1  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0
 0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0
-1  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0
 0  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1
-1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0
 0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0  0  0  0
 0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0
-1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0
 0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0  0
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0
 0 -1  0 -1  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0
-1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0  0  0  0  0 -1  0  0  0  0 -1  0  0  0  0  0  0 -1
 0  0 -1  0  0 -1  0  0  0  0 -1  0  0  0  0 -1  0  0  0  0"
    :map3
"-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
-1  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0 -1
-1  0  0  0  0  0  0 -1 -1 -1 -1  0 -1  0  0  0  0 -1  0 -1
-1  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0 -1  0 -1
-1  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1 -1 -1  0 -1
-1  0  0  0 -1  0  0  0 -1  0  0  0  0  0 -1  0  0 -1  0 -1
-1  0  0 -1  0  0  0 -1  0 -1 -1 -1 -1 -1  0  0  0 -1  0 -1
-1  0 -1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1 -1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1
-1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0 -1
-1  0 -1 -1 -1 -1  0 -1 -1 -1 -1 -1 -1 -1 -1 -1  0 -1 -1 -1
-1  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0  0 -1
-1  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1
-1  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0  0 -1
-1  0  0  0 -1  0  0  0  0  0  0  0  0 -1  0  0  0  0  0 -1
-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1"
    :map4
"0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0 -1  0
 0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0
-1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0
 0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1
 0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0 -1  0  0  0
 0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0
 0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0
-1 -1  0  0 -1  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0 -1 -1  0  0 -1  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0
 0  0  0 -1  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0
 0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1
 0 -1 -1 -1  0 -1  0 -1  0  0 -1  0 -1  0  0  0  0 -1  0  0  0 -1 -1 -1  0 -1  0 -1  0  0 -1  0 -1  0  0  0  0 -1  0  0
 0  0  0 -1  0  0 -1  0  0 -1  0  0  0 -1  0 -1  0 -1  0  0  0  0  0 -1  0  0 -1  0  0 -1  0  0  0 -1  0 -1  0 -1  0  0
 0  0  0 -1  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0
-1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0  0 -1
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0 -1 -1 -1 -1 -1 -1 -1 -1  0  0  0  0  0  0 -1  0 -1  0  0  0 -1 -1 -1 -1 -1 -1 -1 -1  0  0  0  0  0  0 -1  0 -1  0  0
 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0
 0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0  0  0 -1  0
 0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0
-1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0
 0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0  0 -1  0  0 -1  0  0 -1  0  0  0  0  0 -1
 0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0 -1  0  0  0
 0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0 -1  0  0  0 -1  0
 0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0
-1 -1  0  0 -1  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0 -1 -1  0  0 -1  0  0  0  0 -1  0  0  0 -1  0  0  0  0  0  0
 0  0  0 -1  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0
 0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1
 0 -1 -1 -1  0 -1  0 -1  0  0 -1  0 -1  0  0  0  0 -1  0  0  0 -1 -1 -1  0 -1  0 -1  0  0 -1  0 -1  0  0  0  0 -1  0  0
 0  0  0 -1  0  0 -1  0  0 -1  0  0  0 -1  0 -1  0 -1  0  0  0  0  0 -1  0  0 -1  0  0 -1  0  0  0 -1  0 -1  0 -1  0  0
 0  0  0 -1  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0 -1  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0
-1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0
 0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0  0 -1  0  0  0 -1  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0  0 -1
 0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0 -1  0 -1  0
 0 -1 -1 -1 -1 -1 -1 -1 -1  0  0  0  0  0  0 -1  0 -1  0  0  0 -1 -1 -1 -1 -1 -1 -1 -1  0  0  0  0  0  0 -1  0 -1  0  0
 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 -1  0  0"
    :map5  ; this map is interesting because if you set the pathfinding heuristic to manhattan, start-pos to (4 1), end-pos to (0 4),
           ; you'll observe a pretty bad failure if you change one line of logic. Basically if you treat the visited list as a
           ; "you can only visit once" list, you'll find a path with cost: 9 along ((4 1) (3 1) (3 0) (2 0) (1 0) (1 1) (0 1) (0 2) (0 3) (0 4))
           ; but if you treat it properly as the algorithm calls for, which is "you can add a neighbor to the open-list again if
           ; it's already on the visisted list, but it must have a lower cost" then you'll see it finds a shorter path
           ; ((4 1) (4 2) (4 3) (3 3) (2 3) (1 3) (0 3) (0 4)) with cost 7.

           ; The issue is that (2 3) is first found when expanding and considering (2 4) with cost 6. (It takes a beeline to the right and up, then hits the wall.)
           ; However (2 3) is also a neighbor of (3 3), which only has a cost of 4. But if the key and-or logic error associated with commit bfeda8a is messed up,
           ; it won't reconsider (2 3) from the shorter path and skip right on to looking at other nodes. Woops.
"0  0  0  0  0  0
 0  0 -1  0 -1  0
 0  0  0  0  0  0
 0  0  0  0  0  0
 0  0  0  0  0  0"

    :map6
"0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"

))

(defun map-to-maze (map-id)
  "Above maps represent a grid where 0s are open cells and -1s are blocked cells.
   Our maze cells are simplified in that a maze cell can have an east wall or a south wall.
   Thus to parse the above into a maze, for each -1, we must take its north and west neighbors and give them walls.
   Alternatively, what is done below because mazes start out with walls everywhere, for each 0, we remove its south
   and east walls if those neighbors are not -1s."
  (let* ((map (getf *maps* map-id))
         (rows-list (cl-ppcre:split "\\n" map))
         ; Parse each row into a vector of integers
         (grid-rows (mapcar (lambda (row-str)
                              (let ((trimmed (string-trim " " row-str)))
                                    (map 'vector #'parse-integer
                                         (cl-ppcre:split "\\s+" trimmed))))
                            rows-list))
         (row-count (length grid-rows))
         (col-count (length (first grid-rows)))
         (grid (make-array (list row-count col-count)))
         (maze-obj (make-instance 'maze :dimensions (list row-count col-count))))

    ; Fill 2D grid array from parsed rows
    (loop for row-num from 0 below row-count
          for row-vector = (nth row-num grid-rows)
          do (loop for col from 0 below col-count
                   do (setf (aref grid row-num col)
                            (aref row-vector col))))


    ; Remove walls for open cells based on neighbors
    (loop for row from 0 below row-count
          do (loop for col from 0 below col-count
                   when (= (aref grid row col) 0) ; current cell is open
                   do (progn
                        ; Remove east wall if eastern neighbor exists and is open
                        (when (and (< col (1- col-count))
                                   (= (aref grid row (1+ col)) 0))
                          (setf (node-east-wall (aref (.maze maze-obj) row col)) nil))
                        ; Remove south wall if southern neighbor exists and is open
                        (when (and (< row (1- row-count))
                                   (= (aref grid (1+ row) col) 0))
                          (setf (node-south-wall (aref (.maze maze-obj) row col)) nil)))))

    maze-obj))

