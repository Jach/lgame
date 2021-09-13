(in-package #:lgame.time)

(annot:enable-annot-syntax)

@export
(defvar *tick-ms* 0
  "Clock state, represents the number of milliseconds since the SDL library initialized.
   Updated by the clock functions.")

@export
(defun clock-start ()
  "Starts/restarts the clock, users should call this before
   entering their main loop and call 'clock-tick at the end
   of each loop."
  (setf *tick-ms* (lgame::sdl-get-ticks)))

@export
(defun clock-time ()
  "Returns the time in milliseconds since the last call to
   'clock-start or 'clock-tick."
  (- (lgame::sdl-get-ticks) *tick-ms*))

@export
(defun clock-tick (&optional fps-limit)
  "Ticks the clock. If 'fps-limit is specified,
   uses sdl-delay to wait the remainder of the frame time
   needed so that the game loop does not exceed the fps-limit.
   e.g. if 'fps-limit is 60, and a frame takes 10ms, this will
   cause the main loop to sleep for the remaining 6ms.
   Returns the duration amount (i.e. 'clock-time) from BEFORE any
   time spent delaying, allowing for a measure of frame duration
   independent of the delay."
  (let* ((frame-duration (clock-time))
         (millis-per-frame-limit (and fps-limit (/ 1000.0 fps-limit))))
    (when (and millis-per-frame-limit (< frame-duration millis-per-frame-limit))
      (lgame::sdl-delay (truncate (- millis-per-frame-limit frame-duration))))
    (clock-start)
    frame-duration))
