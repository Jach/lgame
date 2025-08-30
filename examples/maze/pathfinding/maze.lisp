#|
Interactively mutated from the original maze.lisp, includes pathfinding logic.

Hit 'f' to 'find' and draw the path between the red block and the blue block. (If there is no path, nothing happens.)

Left click a cell in the maze to move the red block there, (Starting position.)
Right click a cell in the maze to move the blue block there. (Ending position.)

The new path won't update until you hit 'f' again.

Hit 'r' to reset the maze.

Hit 0 to set the maze-mode to randomly generated on reset.
Hit 1 to set the maze-mode to draw map1 from custom-maps.lisp on reset.
Hit 2 to set the maze-mode to draw map2 from custom-maps.lisp on reset.
Hit 3 to set the maze-mode to draw map3 from custom-maps.lisp on reset.
Hit 4 to set the maze-mode to draw map4 from custom-maps.lisp on reset.
Hit 5 to set the maze-mode to draw map5 from custom-maps.lisp on reset.
Hit 6 to set the maze-mode to draw map6 from custom-maps.lisp on reset.

Hit 's' to 'step' the path finding. This will advance the main pathfinding loop
by one. You'll see squares color in pink to represent being or having been on
the open-list, and a square colored yellow to indicate it's the top of the
open-list and next-candidate to check for whether it's the goal.


If playing with this interactively from your slime editor, I suggest this order of operations:
* Evaluate just the defpackage below
* compile-and-load maze-class.lisp
* compile-and-load custom-maps.lisp
* compile-and-load this file
* run (main)

|#

;; quicklisp preamble and quickloading for script usage
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lgame)
  (ql:quickload :livesupport)
  (ql:quickload :alexandria))

(defpackage #:lgame.example.maze-pathfinding
  (:use #:cl)
  (:import-from #:lgame.pathfinding
                #:A*
                #:compute-path
                #:found-shortest-path
                #:.start-pos
                #:.end-pos
                #:goal-row
                #:goal-col))

(in-package #:lgame.example.maze-pathfinding)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))
(load (merge-pathnames "maze-class.lisp" *source-dir*))
(load (merge-pathnames "custom-maps.lisp" *source-dir*))

(defparameter *maze-mode* 0
  "0 = random generation, 1-4 = use corresponding map")
(defparameter *running?* t)

(defparameter *size* '(800 600)
  "Screen size")
(defparameter *dimensions* '(30 40)
  "Maze dimensions")

(defvar *maze-obj* nil)
(defvar *maze-texture* nil)

(defvar *pathfinder* nil)
(defvar *pathfinder-step-started* nil)
(defparameter *print-path?* nil)

(defun init ()
  (lgame.display:create-centered-window "Maze" (first *size*) (second *size*))
  (lgame.display:create-renderer))

(defun start ()
  (restart-game)
  (game-loop))

(defun restart-game ()
  (setf *maze-obj* (make-instance 'maze :dimensions *dimensions*))
  (case *maze-mode*
    (0 (generate *maze-obj*)) ; random generation
    (1 (setf *maze-obj* (map-to-maze :map1)))
    (2 (setf *maze-obj* (map-to-maze :map2)))
    (3 (setf *maze-obj* (map-to-maze :map3)))
    (4 (setf *maze-obj* (map-to-maze :map4)))
    (5 (setf *maze-obj* (map-to-maze :map5)))
    (6 (setf *maze-obj* (map-to-maze :map6)))
    (t (generate *maze-obj*))) ; fallback to random
  (print *maze-obj*)
  (draw-maze)
  (setf *pathfinder-step-started* nil)
  (setf *pathfinder* (make-instance 'lgame.pathfinding::A*
                                    :size (.dims *maze-obj*)
                                    :end-pos (list (1- (first (.dims *maze-obj*))) ; bottom-right
                                                   (1- (second (.dims *maze-obj*))))
                                    :start-pos '(0 0) ; top-left
                                    :heuristic :manhattan
                                    :neighbor-fn (lambda (location)
                                                   (finished-valid-neighbors (.maze *maze-obj*) location))
                                    )))

;(setf (lgame.pathfinding:.heuristic *pathfinder*) :zero)
;(setf (lgame.pathfinding:.heuristic *pathfinder*) :octile)
;(setf (lgame.pathfinding:.heuristic *pathfinder*) :manhattan)

(defmacro with-renderer-info ((info) &body body)
  (let ((sz (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-renderer-info))))
    `(cffi:with-foreign-pointer (,info ,sz)
       ,@body)))

(defun draw-maze ()
  "Creates a texture containing the drawn maze data"
  (when (and *maze-texture* (autowrap:valid-p *maze-texture*))
    (sdl2:destroy-texture *maze-texture*))
  (setf *maze-texture* (sdl2:create-texture lgame:*renderer* lgame::+sdl-pixelformat-rgba8888+ lgame::+sdl-textureaccess-target+ (first *size*) (second *size*)))
  (sdl2:set-render-target lgame:*renderer* *maze-texture*)
  (sdl2:set-render-draw-color lgame:*renderer* 255 255 255 255)
  (sdl2:render-fill-rect lgame:*renderer* lgame:*screen-rect*) ; bg
  (sdl2:set-render-draw-color lgame:*renderer* 0 0 0 255)
  (sdl2:render-draw-rect lgame:*renderer* lgame:*screen-rect*) ; border

  (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
    (loop for y below rows do
        (loop for x below cols do
              (if (node-south-wall (aref (.maze *maze-obj*) y x))
                  (sdl2:render-draw-line lgame:*renderer*
                                         (truncate (* x cell-width)) (truncate (* (1+ y) cell-height))
                                         (truncate (* (1+ x) cell-width)) (truncate (* (1+ y) cell-height))))
              (if (node-east-wall (aref (.maze *maze-obj*) y x))
                  (sdl2:render-draw-line lgame:*renderer*
                                         (truncate (* (1+ x) cell-width)) (truncate (* y cell-height))
                                         (truncate (* (1+ x) cell-width)) (truncate (* (1+ y) cell-height)))))))

  (sdl2:set-render-target lgame:*renderer* nil)
  (values))

(defun get-draw-dims ()
  "Returns as multiple-values the maze's rows, cols, cell-width, and cell-height. Useful for drawing helpers."
  (let* ((rows (first (.dims *maze-obj*)))
         (cols (second (.dims *maze-obj*)))
         (cell-width (/ (first *size*) cols))
         (cell-height (/ (second *size*) rows)))
    (values rows cols cell-width cell-height)))

(defun game-loop ()
  (lgame.time:clock-start)
  (setf *running?* t)
  (loop while *running?* do
        (livesupport:continuable
          (game-tick))))

(defun game-tick ()
  (lgame.event:do-event (event)

    (when (find (lgame.event:event-type event) `(,lgame::+sdl-quit+)); ,lgame::+sdl-keydown+))
      (setf *running?* nil))

    (when (= (lgame.event:event-type event) lgame::+sdl-keydown+)
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-escape+)
        (setf *running?* nil))

      (when (and (= (lgame.event:key-scancode event) lgame::+sdl-scancode-f+)
                 (.start-pos *pathfinder*)
                 (.end-pos *pathfinder*))
        (setf *pathfinder-step-started* nil)
        (when (and (time (lgame.pathfinding::compute-path *pathfinder*))
                   *print-path?*)
            (format t "Found path: ~a~%Cost: ~a~%" (found-shortest-path *pathfinder*) (lgame.pathfinding::seen-path-node-best-real-cost
                                                                                        (aref (lgame.pathfinding::.seen-nodes *pathfinder*) (goal-row *pathfinder*) (goal-col *pathfinder*))))))

      (when (and (= (lgame.event:key-scancode event) lgame::+sdl-scancode-s+)
                 (.start-pos *pathfinder*)
                 (.end-pos *pathfinder*))
        (when (and (lgame.pathfinding::compute-path *pathfinder* :compute-in-single-step? nil :new-request? (not *pathfinder-step-started*))
                   *print-path?*)
          (format t "Found path: ~a~%Cost: ~a~%" (found-shortest-path *pathfinder*) (lgame.pathfinding::seen-path-node-best-real-cost
                                                                                      (aref (lgame.pathfinding::.seen-nodes *pathfinder*) (goal-row *pathfinder*) (goal-col *pathfinder*)))))
        (setf *pathfinder-step-started* t))

      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-0+)
        (setf *maze-mode* 0))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-1+)
        (setf *maze-mode* 1))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-2+)
        (setf *maze-mode* 2))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-3+)
        (setf *maze-mode* 3))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-4+)
        (setf *maze-mode* 4))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-5+)
        (setf *maze-mode* 5))
      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-6+)
        (setf *maze-mode* 6))

      (when (= (lgame.event:key-scancode event) lgame::+sdl-scancode-r+)
        (restart-game)))

    (when (= (lgame.event:event-type event) lgame::+sdl-mousebuttondown+)
      (let* ((button (lgame.event:ref event :button :button))
             (x (lgame.event:ref event :button :x))
             (y (lgame.event:ref event :button :y))

             (rows (first (.dims *maze-obj*)))
             (cols (second (.dims *maze-obj*)))
             (cell-width (/ (first *size*) cols))
             (cell-height (/ (second *size*) rows))

             (clicked-row (truncate y cell-height))
             (clicked-col (truncate x cell-width)))
        (setf *pathfinder-step-started* nil)
        (cond
          ((= button lgame::+sdl-button-left+)
           (setf (.start-pos *pathfinder*) (list clicked-row clicked-col)))
          ((= button lgame::+sdl-button-right+)
           (setf (.end-pos *pathfinder*) (list clicked-row clicked-col))))))
    )
  (sdl2:render-clear lgame:*renderer*)
  (sdl2:render-copy lgame:*renderer* *maze-texture*)

  (when (.start-pos *pathfinder*)
    (sdl2:set-render-draw-color lgame:*renderer* 255 0 0 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (lgame.rect::with-rect (r (+ 2 (* cell-width (second (.start-pos *pathfinder*)))) (+ 2 (* cell-height (first (.start-pos *pathfinder*)))) (- cell-width 4) (- cell-height 4))
        (sdl2:render-fill-rect lgame:*renderer* r))))

  (when (.end-pos *pathfinder*)
    (sdl2:set-render-draw-color lgame:*renderer* 0 0 255 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (lgame.rect::with-rect (r (+ 2 (* cell-width (second (.end-pos *pathfinder*)))) (+ 2 (* cell-height (first (.end-pos *pathfinder*)))) (- cell-width 4) (- cell-height 4))
        (sdl2:render-fill-rect lgame:*renderer* r))))

  (when *pathfinder-step-started*
    (sdl2:set-render-draw-color lgame:*renderer* 255 80 255 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (loop for row below rows do
            (loop for col below cols do
                  (when (aref (lgame.pathfinding::.seen-nodes *pathfinder*) row col)
                    (lgame.rect::with-rect (r (+ 4 (* cell-width col)) (+ 4 (* cell-height row)) (- cell-width 8) (- cell-height 8))
                      (sdl2:render-fill-rect lgame:*renderer* r)))))

      (alexandria:when-let ((best-node (and (not (found-shortest-path *pathfinder*))
                                            (lgame.data-structures:priority-queue-top (lgame.pathfinding::.open-nodes *pathfinder*)))))
        (sdl2:set-render-draw-color lgame:*renderer* 255 255 0 255)
        (let ((row (lgame.pathfinding::open-path-node-row best-node))
              (col (lgame.pathfinding::open-path-node-col best-node)))
          (lgame.rect::with-rect (r (+ 4 (* cell-width col)) (+ 4 (* cell-height row)) (- cell-width 8) (- cell-height 8))
            (sdl2:render-fill-rect lgame:*renderer* r))))))

  (when (found-shortest-path *pathfinder*)
    (sdl2:set-render-draw-color lgame:*renderer* 0 255 0 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (loop for waypoint in (found-shortest-path *pathfinder*) do
            (lgame.rect::with-rect (r (+ 4 (* cell-width (second waypoint))) (+ 4 (* cell-height (first waypoint))) (- cell-width 8) (- cell-height 8))
              (sdl2:render-fill-rect lgame:*renderer* r)))))


  (sdl2:render-present lgame:*renderer*)
  (livesupport:update-repl-link)
  (lgame.time:clock-tick 10))

(defun main ()
  (lgame:init)
  (unwind-protect
    (progn
      (init)
      (start))

    (cleanup)))

(defun cleanup ()
  (sdl2:destroy-texture *maze-texture*)
  (lgame:quit))


(eval-when (:execute)
  (main))
