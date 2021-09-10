#|
Routines to help with time
|#
(in-package #:lgame)

(annot:enable-annot-syntax)

(defvar *prev-frame-time* 0.0)

(defparameter +FPS+ 60)

(defparameter +millis-per-frame+ (/ 1000.0 +FPS+))

@export
(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

@export
(defun init-dt ()
  (setf *prev-frame-time* (now)))

@export
(defun update-dt ()
  "Called at the end of the frame, sets global *dt* to the frame duration.
   Should be consistently 1/+FPS+, but on slower systems it could be larger
   due to poor framerate."
  (setf *dt* (- (now) *prev-frame-time*))
  (setf *prev-frame-time* (now)))

@export
(defun sleep-rest-of-frame (tick-start)
  "If the time between now and tick-start is less than +millis-per-frame+,
   sleep so that our frame rate is capped to +FPS+"
  (let ((frame-dur (- (sdl-get-ticks) tick-start)))
    (when (< frame-dur +millis-per-frame+)
      (sdl-delay (floor (- +millis-per-frame+ frame-dur))))))

