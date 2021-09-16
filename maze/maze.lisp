#|
Based on https://github.com/Jach/PyMaze

Differences:
* Just draws the maze, doesn't have player logic.
* No command line args
* Somewhat smarter generation logic, pops from stack instead of just getting the last value, and
  uses cell locations so it doesn't have to loop and lookup the position.
* On my machine dimensions can go up to 350x350, 351x351 overflows the stack. The generation
  time at such a large scale is still instant though.
* For now I leave it as an exercise to the reader to turn it into an iterative version with no limit.
  It'd also be interesting to demo A* pathfinding with this.
* Supports interactive generation, e.g. load the file and run main, then you can run something like

(let ((maze::*dimensions* '(100 100)))
  (maze::restart-game))

or just (restart-game) on its own, and generate new mazes to your heart's content.
You can also print out an ASCII version of the maze by printing a maze object like *maze-obj*,
done by default in restart-game.

|#

(push (merge-pathnames "../lgame/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload :lgame)
(ql:quickload :livesupport)
(ql:quickload :alexandria)

(defpackage #:maze
  (:use :cl))
(in-package #:maze)

(defparameter *main-dir* (directory-namestring *load-truename*))
(load (merge-pathnames "maze-class.lisp" *main-dir*))

(defparameter *running?* t)

(defparameter *size* '(800 600)
  "Screen size")
(defparameter *dimensions* '(30 40)
  "Maze dimensions")

(defvar *maze-obj* nil)
(defvar *maze-texture* nil)

(defun init ()
  (lgame.loader:create-texture-loader *main-dir*)
  (lgame.display:create-window "Maze" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+
                               (first *size*) (second *size*))
  (lgame.display:create-renderer))

(defun start ()
  (restart-game)
  (game-loop))

(defun restart-game ()
  (setf *maze-obj* (make-instance 'maze :dimensions *dimensions*))
  (generate *maze-obj*)
  (print *maze-obj*)
  (draw-maze))

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

  (let* ((rows (first (.dims *maze-obj*)))
         (cols (second (.dims *maze-obj*)))
         (cell-width (/ (first *size*) cols))
         (cell-height (/ (second *size*) rows)))

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

(defun game-loop ()
  (lgame.time:clock-start)
  (setf *running?* t)
  (loop while *running?* do
        (livesupport:continuable
          (game-tick))))

(defun game-tick ()
  (lgame.event:do-event (event)
    (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+))
        (setf *running?* nil)))
  (sdl2:render-clear lgame:*renderer*)
  (sdl2:render-copy lgame:*renderer* *maze-texture*)
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
