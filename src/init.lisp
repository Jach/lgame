(in-package #:lgame)

(define-condition sdl-error (error)
  ((msg :initarg :msg :accessor sdl-error-msg)))

(defun show-sdl-error (msg)
  (format t "SDL Error: ~a~%" msg)
  (unless (zerop (lgame::sdl-show-simple-message-box
                   lgame::+sdl-messagebox-error+
                   "SDL Error"
                   msg
                   nil))
    (format t "SDL Error: Also could not display error message box...~%")))

(defun init ()
  (handler-bind
    ((sdl-error (lambda (e)
                  (show-sdl-error (lgame::sdl-error-msg e))
                  (return-from init))))

    (unless (zerop
              (lgame::sdl-init lgame::+sdl-init-everything+))
      (error "Could not initialize SDL"))

    (sdl2-image:init '(:png :jpg))
    (sdl2-mixer:init :mp3); :wave)
    (sdl2-mixer:open-audio 44100 :s16sys 2 4096)

    (sdl2-ttf:init)))

(defun quit ()
  (lgame.font:unload-fonts)
  (sdl2-ttf:quit)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  (sdl2-image:quit)

  (when *texture-loader*
    (lgame:unload-textures *texture-loader*)
    (setf *texture-loader* nil))

  (unless (lgame::null-ptr? *renderer*)
    (lgame::sdl-destroy-renderer *renderer*)
    (setf *renderer* nil))
  (unless (lgame::null-ptr? *screen*)
    (lgame::sdl-destroy-window *screen*)
    (setf *screen* nil))
  (sdl2:free-rect *screen-rect*)
  (setf *screen-rect* nil)
  (lgame::sdl-quit))
