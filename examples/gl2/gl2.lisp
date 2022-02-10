#|
More complicated example of using SDL2 with OpenGL,
this replicates https://lazyfoo.net/tutorials/SDL/51_SDL_and_modern_opengl/index.php
to show a "modern" render process using a shader.
You probably will want to check out https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp
for a saner way of dealing with the vertex and index arrays.
|#

(ql:quickload :lgame)
(ql:quickload :livesupport)
(ql:quickload :cl-opengl)

(defpackage #:lgame.example.gl2
  (:use #:cl)
  (:export #:main))
(in-package #:lgame.example.gl2)

(defvar *running?* t)

(defvar *gl-context* nil)
(defvar *gl-program-ID* 0)

(defvar *vertex-pos-2d-location* -1) ; vertex attribute/position
(defvar *vbo* 0) ; vertex buffer object (what to draw)
(defvar *ibo* 0) ; index buffer object (draw order)

(defvar *vertex-array* nil)
(defvar *index-array* nil)

(defun init ()
  (lgame:init)

  ; use OpenGL 3.1 core
  (sdl2:gl-set-attrs :context-major-version 3
                     :context-minor-version 1
                     :context-profile-mask lgame::+sdl-gl-context-profile-core+)

  (lgame.display:create-centered-window "GL2" 800 600 lgame::+sdl-window-opengl+)
  (lgame.display:create-renderer)

  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl2-ffi.functions:sdl-gl-get-proc-address)
  (setf *gl-context* (lgame::sdl-gl-create-context lgame:*screen*))

  ; use vsync
  (sdl2:gl-set-swap-interval 1)


  (setf *gl-program-ID* (gl:create-program))

  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (vertex-shader-src "
                           #version 140
                           in vec2 LVertexPos2D;
                           void main() {
                           gl_Position = vec4(LVertexPos2D.x, LVertexPos2D.y, 0, 1);
                           }
                           "
                           ))
    (gl:shader-source vertex-shader (list vertex-shader-src))
    (gl:compile-shader vertex-shader)
    (cffi:with-foreign-object (compiled? '(:pointer :int))
      (%gl:get-shader-iv vertex-shader :compile-status compiled?)
      (format t "Vertex shader compiled? ~a~%" (cffi:mem-ref compiled? :int)))

    (gl:attach-shader *gl-program-ID* vertex-shader)

    (let ((fragment-shader (gl:create-shader :fragment-shader))
          (fragment-shader-src "
                               #version 140
                               out vec4 LFragment;
                               void main() {
                                 LFragment = vec4(1.0, 1.0, 1.0, 1.0);
                               }
                               "
                               ))
      (gl:shader-source fragment-shader (list fragment-shader-src)) ; eh?
      (gl:compile-shader fragment-shader)
      (cffi:with-foreign-object (compiled? '(:pointer :int))
        (%gl:get-shader-iv fragment-shader :compile-status compiled?)
        (format t "Fragment shader compiled? ~a~%" (cffi:mem-ref compiled? :int)))

      (gl:attach-shader *gl-program-ID* fragment-shader)

      ; link
      (gl:link-program *gl-program-ID*)
      (cffi:with-foreign-object (linked? '(:pointer :int))
        (%gl:get-program-iv *gl-program-ID* :link-status linked?)
        (format t "Program linked? ~a~%" (cffi:mem-ref linked? :int)))

      ; get location for our glsl input variable:
      (setf *vertex-pos-2d-location* (gl:get-attrib-location *gl-program-ID* "LVertexPos2D"))
      ; check if -1 for err
      (gl:clear-color 0 0.5 0 1)
      (let ((vertex-data #(-0.5 -0.5
                           0.5 -0.5
                           0.5 0.5
                           -0.5 0.5)) ; quad
            (index-data #(0 1 2 3)))
        (setf *vertex-array* (gl:alloc-gl-array :float 8))
        (setf *index-array* (gl:alloc-gl-array :unsigned-int 4))
        (dotimes (i (length vertex-data))
          (setf (gl:glaref *vertex-array* i) (aref vertex-data i)))
        (dotimes (i (length index-data))
          (setf (gl:glaref *index-array* i) (aref index-data i)))
        (setf *vbo* (gl:gen-buffers 1))
        (gl:bind-buffer :array-buffer (first *vbo*))
        (gl:buffer-data :array-buffer :static-draw *vertex-array*)

        (setf *ibo* (gl:gen-buffers 1))
        (gl:bind-buffer :element-array-buffer (first *ibo*))
        (gl:buffer-data :element-array-buffer :static-draw *index-array*))



      )))


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
  ;(gl:clear-color 1.0 0 1 1)
  (gl:clear :color-buffer-bit)

  (gl:use-program *gl-program-ID*)
  (gl:enable-vertex-attrib-array *vertex-pos-2d-location*)
  ; set vertex data
  (gl:bind-buffer :array-buffer (first *vbo*))
  (gl:vertex-attrib-pointer *vertex-pos-2d-location* 2 :float nil (* 2 (cffi:foreign-type-size :float)) (cffi:null-pointer))
  ; set index data
  (gl:bind-buffer :element-array-buffer (first *ibo*))
  ; render
  (%gl:draw-elements :triangle-fan 4 :unsigned-int (cffi:null-pointer))

  ; disable
  (gl:disable-vertex-attrib-array *vertex-pos-2d-location*)

  (gl:use-program 0)

  (sdl2:gl-swap-window lgame:*screen*)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick 60))

(defun main ()
  (unwind-protect
    (progn
      (init)
      (game-loop))

    (gl:free-gl-array *vertex-array*)
    (gl:free-gl-array *index-array*)
    (gl:delete-program *gl-program-ID*)
    (lgame:quit)))

(eval-when (:execute)
  (main))

