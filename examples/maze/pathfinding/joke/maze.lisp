#|
Interactively modified from the first version of pathfinding/maze.lisp example, this one implemented a 'joke'.
Instead of drawing the path with green blocks, it was formed by a vtuber's neck: https://twitter.com/jachy/status/1441803861522407437/photo/1

I've poorly redrawn the assets to something else so they can be included here.
You can still click to set custom start/end points like the first version, and use f to draw the path, or r to reset with a new maze.
The start/end blocks aren't drawn after the path has been drawn, but they are still set, just re-run the path finder to see.

One visual edge case remains because the final head-neck connector (neck-top)
includes part of the head's hair, which must be oriented with the head. This
could be avoided by not doing that with the final connecting asset, so that the
head can be on a curve with the previous segment. I've left it to retain parity
with the original's limitation.


If playing with this interactively from your slime editor, I suggest this order of operations:
* Evaluate just the defpackage below
* compile-and-load maze-class.lisp
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

(defpackage #:lgame.example.maze-pathfinding-joke
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.pathfinding
                #:A*
                #:compute-path
                #:.start-pos
                #:.end-pos
                #:.waypoint-list))
(in-package #:lgame.example.maze-pathfinding-joke)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))
(load (merge-pathnames "maze-class.lisp" *source-dir*))

(defparameter *running?* t)

(defparameter *size* '(1600 1200)
  "Screen size")
(defparameter *dimensions* '(30 40)
  "Maze dimensions")

(defvar *maze-obj* nil)
(defvar *maze-texture* nil)

(defvar *pathfinder* nil)

(defun init ()
  (lgame.loader:create-texture-loader (merge-pathnames "selen/" *source-dir*))
  (lgame.display:create-centered-window "Maze" (first *size*) (second *size*))
  (lgame.display:create-renderer))

(defun start ()
  ; load selen
  (loop for img in '(:head :neck-bottom :neck-corner-bottomleft :neck-corner-bottomright :neck-corner-topleft :neck-corner-topright :neck-horiz :neck-top :neck-vert) do
        (lgame.loader:get-texture img))
  (restart-game)
  (game-loop))

(defun restart-game ()
  (setf *maze-obj* (make-instance 'maze :dimensions *dimensions*))
  (generate *maze-obj*)
  (print *maze-obj*)
  (draw-maze)
  (setf *pathfinder* (make-instance 'lgame.pathfinding::A*
                                     :size *dimensions*
                                     :end-pos (list (1- (first *dimensions*)) ; bottom-right
                                                      (1- (second *dimensions*)))
                                     :start-pos '(0 0) ; top-left
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
  (sdl2:set-render-draw-color lgame:*renderer* 0 0 0 255)
  (sdl2:render-fill-rect lgame:*renderer* lgame:*screen-rect*) ; bg
  (sdl2:set-render-draw-color lgame:*renderer* 255 255 255 255)
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
        (lgame.pathfinding::compute-path *pathfinder*))
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

  (when (and (.start-pos *pathfinder*) (not (.waypoint-list *pathfinder*)))
    (sdl2:set-render-draw-color lgame:*renderer* 255 0 0 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (lgame.rect::with-rect (r (+ 2 (* cell-width (second (.start-pos *pathfinder*)))) (+ 2 (* cell-height (first (.start-pos *pathfinder*)))) (- cell-width 4) (- cell-height 4))
        (sdl2:render-fill-rect lgame:*renderer* r))))

  (when (and (.end-pos *pathfinder*) (not (.waypoint-list *pathfinder*)))
    (sdl2:set-render-draw-color lgame:*renderer* 0 0 255 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (lgame.rect::with-rect (r (+ 2 (* cell-width (second (.end-pos *pathfinder*)))) (+ 2 (* cell-height (first (.end-pos *pathfinder*)))) (- cell-width 4) (- cell-height 4))
        (sdl2:render-fill-rect lgame:*renderer* r))))

  (when (.waypoint-list *pathfinder*)
    (sdl2:set-render-draw-color lgame:*renderer* 0 255 0 255)
    (multiple-value-bind (rows cols cell-width cell-height) (get-draw-dims)
      (declare (ignore rows cols))
      (let ((waypoints (make-array (length (.waypoint-list *pathfinder*)) :initial-contents (.waypoint-list *pathfinder*))))
        (loop for waypoint across waypoints
              for i from 0
              with angle = 0.0d0
              with img
              do
              (setf img :neck-vert)
              ; determine angle of current waypoint with next waypoint to rotate current correctly
              (when (< i (1- (length waypoints)))
                (setf angle (case (direction waypoint (aref waypoints (1+ i)))
                              (:north 0.0d0)
                              (:south 180.0d0)
                              (:east 90.0d0)
                              (:west -90.0d0)))
                ; determine if we're at a corner... need to compare to previous waypoint?
                (when (> i 0)
                  (let ((dir (direction waypoint (aref waypoints (1+ i))))
                        (prev-dir (direction (aref waypoints (1- i)) waypoint)))
                    (case prev-dir
                      (:north ; we're north of prev cell
                        (if (eql dir :west) ; need top-right corner
                            (setf img :neck-corner-topright
                                  angle 0.0d0))
                        (if (eql dir :east)
                            (setf img :neck-corner-topleft
                                  angle 0.0d0)))
                      (:west ; we're west of prev cell
                        (if (eql dir :north) ; need bottom-left
                            (setf img :neck-corner-bottomleft
                                  angle 0.0d0))
                        (if (eql dir :south)
                            (setf img :neck-corner-topleft
                                  angle 0.0d0)))
                      (:east
                        (if (eql dir :north)
                            (setf img :neck-corner-bottomright
                                  angle 0.0d0))
                        (if (eql dir :south)
                            (setf img :neck-corner-topright
                                  angle 0.0d0)))
                      (:south
                        (if (eql dir :east)
                            (setf img :neck-corner-bottomleft
                                  angle 0.0d0))
                        (if (eql dir :west)
                            (setf img :neck-corner-bottomright
                                  angle 0.0d0)))))))
              ;(lgame.display:screenshot-png "/home/kevin/projects/selen-maze.png")

              ; handle last one specially, compare it to one before
              (when (= i (1- (length waypoints)))
                (setf angle (case (direction (aref waypoints (1- i)) waypoint)
                              (:north 0.0d0)
                              (:south 180.0d0)
                              (:east 90.0d0)
                              (:west -90.0d0))))

              (lgame.rect::with-rect (r (* cell-width (second waypoint)) (* cell-height (first waypoint)) cell-width cell-height)
                ; todo, check rounding above rect values instead of truncate to see if that gets rid of one-pixel disconnect at 20x30 size
                (cond
                  ((= i 0) (sdl2:render-copy-ex lgame:*renderer* (lgame.texture:.sdl-texture (lgame.loader:get-texture :neck-bottom)) :dest-rect r :angle angle))
                  ((= i (1- (1- (length waypoints))))
                   (sdl2:render-copy-ex lgame:*renderer* (lgame.texture:.sdl-texture (lgame.loader:get-texture :neck-top)) :dest-rect r :angle angle))
                  ((= i (1- (length waypoints)))
                   (sdl2:render-copy-ex lgame:*renderer* (lgame.texture:.sdl-texture (lgame.loader:get-texture :head)) :dest-rect r :angle angle))
                  (t
                   (sdl2:render-copy-ex lgame:*renderer* (lgame.texture:.sdl-texture (lgame.loader:get-texture img)) :dest-rect r :angle angle)))
                )))))



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
