(in-package #:lgame)

(define-condition lgame-error (error)
  ((msg :initarg :msg :accessor lgame-error-msg))
  (:report (lambda (condition stream)
             (format stream "lgame encountered an error: ~a" (lgame-error-msg condition)))))

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
    ((lgame-error (lambda (e)
                    (show-sdl-error (lgame-error-msg e))
                    (return-from init))))

    (unless (zerop
              (lgame::sdl-init lgame::+sdl-init-everything+))
      (error (format nil "Could not initialize SDL: ~a" (sdl2-ffi.functions:sdl-get-error))))

    (sdl2-image:init '(:png :jpg))
    (sdl2-mixer:init :mp3); :wave)
    (sdl2-mixer:open-audio 44100 :s16sys 2 4096)

    (lgame.font.ffi:ttf-init)
    ))

(defun quit ()
  (lgame.mouse:cleanup-cursors)

  (lgame.font:unload-fonts)
  (lgame.font.ffi:ttf-quit)
  (lgame.font:destroy-rendered-text-textures)

  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  (sdl2-image:quit)

  (when (lgame.time:clock-running?)
    (lgame.time:clock-stop))

  (when *texture-loader*
    (lgame.loader:unload-textures *texture-loader*)
    (setf *texture-loader* nil))

  (unless (lgame::null-ptr? *renderer*)
    (lgame::sdl-destroy-renderer *renderer*)
    (setf *renderer* nil))
  (unless (lgame::null-ptr? *screen*)
    (lgame::sdl-destroy-window *screen*)
    (setf *screen* nil))
  (when *screen-rect*
    (sdl2:free-rect *screen-rect*))
  (setf *screen-rect* nil)
  (setf *screen-box* nil)
  (lgame::sdl-quit))
