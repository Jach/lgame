(in-package #:lgame.time)

(defvar *tick-ms* 0
  "Clock state, represents the number of milliseconds since the SDL library initialized.
   Updated by the clock functions.")

(defvar *tick-us* 0
  "Meant for internal use with SBCL, microsecond precision, represents us since unix epoch, or since SDL library initialized if not using SBCL.")

(declaim (type double-float *last-frame-duration*))
(defvar *last-frame-duration* 0.0d0
  "Updated to the first return value of 'clock-tick, meant to ease access to the previous frame's duration in milliseconds")

(declaim (type double-float *last-any-delay*))
(defvar *last-any-delay* 0.0d0
  "Updated to the second return value of 'clock-tick, meant to ease access to the previous frame's delay amount, if any. See 'clock-tick for possible values.")

#+sbcl
(declaim (inline now-us)
         (ftype (function () (values (signed-byte 64))) now-us))
#+sbcl
(defun now-us ()
  "Microseconds since unix epoch"
  (multiple-value-bind (s us) (sb-ext:get-time-of-day)
    (+ us (* #.(truncate 1e6) s))))

#+sbcl
(defun now-ns ()
  "Nanoseconds since unix epoch"
  (multiple-value-bind (s ns) (sb-unix:clock-gettime sb-unix:clock-realtime)
    (+ ns (* #.(truncate 1e9) s))))

(defvar %running? nil
  "T if the lgame clock has been started/restarted and not stopped.")

(defun clock-start ()
  "Starts/restarts the clock, users should call this before
   entering their main loop and call 'clock-tick at the end
   of each loop."
  (setf %running? T
        *tick-ms* (lgame::sdl-get-ticks)
        *last-frame-duration* 0.0d0
        *last-any-delay* 0.0d0)

  (setf *tick-us* #+sbcl (now-us) #-sbcl (* 1000 *tick-ms*)))


(defun clock-stop ()
  "Flags that the clock should be considered stopped."
  (setf %running? nil))

(defun clock-running? ()
  "Useful as a main game loop while condition,
   T if the clock has been started/restarted and not stopped."
  %running?)

(defun clock-time ()
  "Returns the time in milliseconds since the last call to
   'clock-start or 'clock-tick.
   Note: will be a floating point value on SBCL because sub-ms precision
   is available."
  #-sbcl
  (- (lgame::sdl-get-ticks) *tick-ms*)
  #+sbcl
  (/ (- (now-us) *tick-us*) 1000.0d0))

(defun clock-tick (&optional fps-limit)
  "Ticks the clock. If 'fps-limit (in seconds) is specified,
   uses sdl-delay (or sleep on sbcl) to wait the remainder of the frame time
   needed so that the game loop does not exceed the fps-limit.
   e.g. if 'fps-limit is 60, and a frame takes 10ms, this will
   cause the main loop to sleep for the remaining 6ms.

   Returns the duration amount in milliseconds (i.e. 'clock-time) from BEFORE any
   time spent delaying, allowing for a measure of frame duration
   independent of the delay.

   As an optional second value, returns the millisecond difference
   between the frame limit and the frame duration.
   If fps-limit wasn't passed, or the frame duration matches the limit, it will be 0.
   If the frame duration is exceeding the limit, it will be negative.
   If there was a delay, it will be positive.

   Note on SBCL, values will be floats due to microsecond precision."
  (let* ((frame-duration (clock-time))
         (millis-per-frame-limit (and fps-limit (/ 1000.0 fps-limit)))
         (any-delay (or (and millis-per-frame-limit (- millis-per-frame-limit
                                                       frame-duration))
                        0d0)))
    (when (and millis-per-frame-limit (< frame-duration
                                         millis-per-frame-limit))
      #+sbcl
      (sleep (/ any-delay 1000.0d0))
      #-sbcl
      (lgame::sdl-delay (truncate any-delay)))
    (setf *tick-ms* (lgame::sdl-get-ticks))
    (setf *tick-us* #+sbcl (now-us) #-sbcl (* 1000 *tick-ms*))
    (setf *last-frame-duration* frame-duration
          *last-any-delay* any-delay)
    (values frame-duration any-delay)))

(defun dt ()
  "Returns the delta-time between frames, inclusive of any delays from an fps-limit, in floating point unit seconds.
   If the time is too large, returns 0.25 to try and avoid a potential physics explosion...
   Note for the very first frame (frame 0), there is no previous frame, so the delta time is 0.
   Thus be careful if relying on dt for certain calculations on the first frame.
   After a frame has ended and clock-tick has been called, dt will be the correct delta time."
  (let ((total-time (+ *last-frame-duration* *last-any-delay*)))
    (min (* total-time 1d-3) 0.25d0)))

