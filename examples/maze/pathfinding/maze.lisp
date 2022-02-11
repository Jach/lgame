#|
Interactively mutated from the original maze.lisp, includes pathfinding logic.
Hit 'f' to 'find' and draw the path between the red block and the blue block.
Left click a cell in the maze to move the red block there,
Right click a cell in the maze to move the blue block there.
The new path won't update until you hit 'f' again.
|#

(ql:quickload :lgame)
(ql:quickload :livesupport)
(ql:quickload :alexandria)

(defpackage #:lgame.example.maze-pathfinding
  (:use #:cl)
  (:import-from #:lgame.pathfinding
                #:A*
                #:compute-path
                #:.start-pos
                #:.end-pos
                #:.waypoint-list))
(in-package #:lgame.example.maze-pathfinding)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))
(load (merge-pathnames "maze-class.lisp" *source-dir*))

(defparameter *running?* t)

(defparameter *size* '(800 600)
  "Screen size")
(defparameter *dimensions* '(30 40)
  "Maze dimensions")

(defvar *maze-obj* nil)
(defvar *maze-texture* nil)

(defvar *pathfinder* nil)

(defun init ()
  (lgame.display:create-centered-window "Maze" (first *size*) (second *size*))
  (lgame.display:create-renderer))

(defun start ()
  (restart-game)
  (game-loop))

(defun restart-game ()
  (setf *maze-obj* (make-instance 'maze :dimensions *dimensions*))
  (generate *maze-obj*)
  (print *maze-obj*)
  (draw-maze)
  (setf *pathfinder* (make-instance 'lgame.pathfinding::A*
                                     :size *dimensions*
                                     :start-pos (list (1- (first *dimensions*)) ; bottom-right
                                                      (1- (second *dimensions*)))
                                     :end-pos '(0 0) ; top-left
                                     :neighbor-fn (lambda (location)
                                                    (finished-valid-neighbors (.maze *maze-obj*) location))
                                     )))

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
        (lgame.pathfinding::compute-path *pathfinder* :single-step? t :new-request? t))
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

  (when (.waypoint-list *pathfinder*)
    (sdl2:set-render-draw-color lgame:*renderer* 0 255 0 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (loop for waypoint in (.waypoint-list *pathfinder*) do
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
