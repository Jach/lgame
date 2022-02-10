#|
Based on https://github.com/pygame/pygame/blob/main/examples/vgrade.py
Displays a random-colored gradient twice per second.

Differences:

The original uses pygame's surfarray combined with the blazing fast numpy to
showcase what's fundamentally a software renderer design: create an array of
pixels that you update, and every frame draw that array to the screen. Lisp has
some blazing fast array libs too, but here we're just going to use a plain CL
array and explicit loops, go pixel-by-pixel instead of making a single column
and repeating it like the pygame version does, and every frame copy our pixel
data to a foreign array (RAM) and from the foreign memory to a Texture (GPU
memory) which is then rendered.

For some performance comparisons, on my machine,
using pygame 2.0.1 (SDL 2.0.14, Python 3.9.5)
it takes about 2-3ms (once 5ms) per gradient. Pretty good!

With SBCL, this program takes around 120-135ms (once 140ms) per gradient, of
which about 85ms is spent just copying the lisp array to the foreign memory.
This did not change whether using the cffi:with-foreign-array macro or creating a
separate cffi:foreign-array-alloc buffer-ptr outside the loop and passing it in,
updating it with cffi:lisp-array-to-foreign.

While it's surely possible to optimize this, hopefully it should casually
dissaude you from attempting this sort of software rendering design that works
pixel by pixel unless you have something that can tolerate 8 FPS. For example, an
image processing program whose only job is to do processing and display the
output. And the best way to optimize this would surely be to use a library that
manages arrays natively so that there's no overhead to convert from lisp data to
the data expected by SDL...

Before you go that far though you may want to consider how far you can get just
by rendering filled rects. In the main function below, swap which version of
display-gradient is used to the one that has :use-rects. What this does
differently is that it loops through each row of the screen, updates the color
to draw, and calls render-fill-rect to draw a one-pixel high rect of color
across row's columns. This is so fast that SDL's millisecond precision is no
longer good enough to time it, and exposed a divide-by-zero bug in my stopwatch
macro. You can use with-stopwatch-unix instead to get microsecond precision,
which reveals each gradient takes about half a millisecond, with occasional spikes
to 1-1.5ms (perhaps due to a GC).

These results made me curious so I added a third way, which is :use-render-point,
it goes pixel-by-pixel too but instead of updating a Lisp array it just renders
immediately with sdl2:render-draw-point. This ended up being 30ms, which is quite
a bit better, but still clearly not a great idea.

|#
(defpackage #:vgrade
  (:use :cl))
(in-package #:vgrade)

(defparameter *main-dir* (directory-namestring *load-truename*))
(defparameter *running?* t)

(ql:quickload :lgame)
(ql:quickload :livesupport)

(defparameter *width* 600)
(defparameter *height* 400)
(defparameter *size* (* *width* *height*))

(defun main (&aux background buffer)
  (lgame:init)
  (lgame.display:create-centered-window "gradient" *width* *height*
                                        (logior lgame::+sdl-window-opengl+ lgame::+sdl-window-borderless+))
  (lgame.display:create-renderer)

  (setf background (sdl2:create-texture lgame:*renderer* lgame::+sdl-pixelformat-rgba8888+ lgame::+sdl-textureaccess-streaming+
                                        *width* *height*))
  ; with this format, pixel colors are in the form #xRRGGBBAA
  (setf buffer (make-array *size* :initial-element 0 :element-type '(unsigned-byte 32)))

  (setf *running?* t)
  (loop while *running?* do
        (livesupport:continuable
          (lgame.event:do-event (event)
            (if (find (lgame.event:event-type event) `(,lgame::+sdl-quit+ ,lgame::+sdl-keydown+ ,lgame::+sdl-mousebuttondown+))
                (setf *running?* nil)))

          (display-gradient background buffer :use-buffer t)
          ;(display-gradient background buffer :use-rects t)
          ;(display-gradient background buffer :use-render-point t)
          (livesupport:update-repl-link)))

  (sdl2:destroy-texture background)

  (lgame:quit))

(defmacro with-stopwatch (&body body)
  (let ((start (gensym)))
    `(let ((,start (lgame::sdl-get-ticks)))
       ,@body
       (let ((end (+ 0.0001 (/ (- (lgame::sdl-get-ticks) ,start) 1000.0))))
         (format t "Gradient: ~a ms (~$fps)~%" (* end 1000) (/ 1 end))))))

#+sbcl
(defmacro with-stopwatch-unix (&Body body)
  (let ((start (gensym)))
    `(let ((,start (nth-value 2 (sb-unix:unix-gettimeofday))))
       ,@body
       (let ((end (+ 0.000001 (/ (- (nth-value 2 (sb-unix:unix-gettimeofday)) ,start) 1000000.0))))
         (format t "Gradient: ~a ms (~$fps)~%" (* end 1000) (/ 1 end))))))

(defun rgb-to-rgba8888 (rgb-array)
  (let ((r (ash (truncate (aref rgb-array 0)) (* 8 3)))
        (g (ash (truncate (aref rgb-array 1)) (* 8 2)))
        (b (ash (truncate (aref rgb-array 2)) 8)))
    (logior r g b #xFF)))
;(format t "~x~%" (rgb-to-rgba8888 #(255 5 255)))
; -> FF05FFFF

(defun add-color (col1 col2)
  "Add col2 to col1"
  (incf (aref col1 0) (aref col2 0))
  (incf (aref col1 1) (aref col2 1))
  (incf (aref col1 2) (aref col2 2)))

(defun display-gradient (texture buffer &key use-buffer use-rects use-render-point)
  (sleep 0.5)
  (with-stopwatch
    (let ((start-color (vector (random 256) (random 256) (random 256)))
          (end-color (vector (random 256) (random 256) (random 256)))
          (cur-color)
          (step-color))
      (setf cur-color start-color)
      (setf step-color (vector (/ (- (aref end-color 0)
                                     (aref start-color 0))
                                  *height*)
                               (/ (- (aref end-color 1)
                                     (aref start-color 1))
                                  *height*)
                               (/ (- (aref end-color 2)
                                     (aref start-color 2))
                                  *height*)))

      (when use-buffer
        (loop for row below *height* do
              (loop for col below *width* do
                    (setf (aref buffer (+ (* row *width*) col)) (rgb-to-rgba8888 cur-color)))
              (add-color cur-color step-color))

        (cffi:with-foreign-array (buffer-ptr buffer (list :array :uint32 *size*))
          (sdl2:update-texture texture nil buffer-ptr (* *width* (cffi:foreign-type-size :uint32)))
          (sdl2:render-copy lgame:*renderer* texture)
          (sdl2:render-present lgame:*renderer*)))

      (when use-rects
        (loop for row below *height* do
              (sdl2:set-render-draw-color lgame:*renderer* (truncate (aref cur-color 0)) (truncate (aref cur-color 1)) (truncate (aref cur-color 2)) 255)
              (lgame.rect:with-rect (rect 0 row *width* 1)
                (sdl2:render-fill-rect lgame:*renderer* rect))
              (add-color cur-color step-color))
        (sdl2:render-present lgame:*renderer*))

      (when use-render-point
        (loop for row below *height* do
              (sdl2:set-render-draw-color lgame:*renderer* (truncate (aref cur-color 0)) (truncate (aref cur-color 1)) (truncate (aref cur-color 2)) 255)
              (loop for col below *width* do
                    (sdl2:render-draw-point lgame:*renderer* col row))
              (add-color cur-color step-color))
          (sdl2:render-present lgame:*renderer*)))))

(eval-when (:execute)
  (main))
