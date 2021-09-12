(in-package #:lgame)

(annot:enable-annot-syntax)

(define-condition sdl-error (error)
  ((msg :initarg :msg :accessor sdl-error-msg)))

(defun show-sdl-error (msg)
  (format t "SDL Error: ~a~%" msg)
  (unless (zerop (sdl-show-simple-message-box
                   +sdl-messagebox-error+
                   "SDL Error"
                   msg
                   nil))
    (format t "SDL Error: Also could not display error message box...~%")))

@export
(defun init ()
  (handler-bind
    ((sdl-error (lambda (e)
                  (show-sdl-error (sdl-error-msg e))
                  (return-from init))))

    (unless (zerop
              (sdl-init +sdl-init-everything+))
      (error "Could not initialize SDL"))

    (sdl2-image:init '(:png :jpg))
    (sdl2-mixer:init :mp3); :wave)
    (sdl2-mixer:open-audio 44100 :s16sys 2 4096)

    (sdl2-ttf:init)))

@export
(defun quit ()
  (unload-fonts)
  (ttf-quit)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  (sdl2-image:quit)

  (when *texture-loader*
    (unload-textures *texture-loader*)
    (setf *texture-loader* nil))

  (unless (null-ptr? *renderer*)
    (sdl-destroy-renderer *renderer*)
    (setf *renderer* nil))
  (unless (null-ptr? *screen*)
    (sdl-destroy-window *screen*)
    (setf *screen* nil))
  (sdl2:free-rect *screen-rect*)
  (setf *screen-rect* nil)
  (sdl-quit))
