(in-package #:lgame.event)

(defmacro with-event ((event) &body body)
  "Helper macro to enable sdl event allocation on the stack.
   Verify with macroexpand this:

(with-event (event)
  (print event))

  against:

(sdl2:with-sdl-event (event)
  (print event))

  or even:

(plus-c:c-let ((event sdl2-ffi:sdl-event :free t))
  (print event))
"
  (let ((size (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-event))))
    `(cffi:with-foreign-pointer (,event  ,size)
       ,@body)))

(defmacro do-event ((event) &body loop-body)
  "Helper macro to iterate through SDL's event list until it is empty,
   binding each SDL_Event to event."
  `(with-event (,event)
     (loop until (zerop (sdl2-ffi.functions:sdl-poll-event ,event))
           do
           ,@loop-body)))

(defmacro ref (event &rest fields)
  `(plus-c:c-ref ,event sdl2-ffi:sdl-event ,@fields))

(defun event-type (event)
  (ref event :type))

(defun key-scancode (event)
  ;(plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :scancode))
  (ref event :key :keysym :scancode))

(defun key-pressed? (&key key any all)
  "If :key is given, checks a single-key for pressed state.
   If :any is given, checks if any in the list are pressed.
   If :all is given, checks that all in the list are pressed.
   Note that this just calls SDL_GetKeyboardState and has the same limitations."
  (let* ((state (nth-value 1 (sdl2-ffi.functions:sdl-get-keyboard-state nil)))
         (test (lambda (key) (= 1 (cffi:mem-aref state :uint8 key)))))
    (if key
        (funcall test key)
        (if any
            (some test any)
            (if all
                (every test all))))))

(defun modifier-pressed? (&key key any all)
  "Works similarly to key-pressed? but checks for modifier keys like shift,
   so expects values like +kmod-shift+ or +kmod-caps+"
  (let* ((state (sdl2-ffi.functions:sdl-get-mod-state))
         (test (lambda (key) (plusp (logand key state)))))
    (if key
        (funcall test key)
        (if any
            (some test any)
            (if all
                (every test all))))))

;;; Joysticks / gamepads

(defun open-gamepads ()
  "Opens any connected gamepads and populates lgame.state:*opened-gamepads* as a map from integer id -> gamepad. Returns the map for convenience."
  (dotimes (id (lgame::sdl-num-joysticks))
    (when (= 1 (lgame::sdl-is-game-controller id))
      (let ((pad (lgame::sdl-game-controller-open id)))
        (unless (lgame:null-ptr? pad)
          (setf (gethash id lgame.state:*opened-gamepads*) pad)))))
  lgame.state:*opened-gamepads*)

(defun gamepad-attached? (&key id gamepad)
  "Checks whether a controller has been opened and is currently attached.
   Takes either an :id argument which is the key of the lgame.state:*opened-gamepads* map, or a :gamepad argument which is a value of the map."
  (alexandria:when-let ((to-check (or gamepad (gethash id lgame.state:*opened-gamepads*))))
    (= 1 (lgame::sdl-game-controller-get-attached to-check))))

(defun close-gamepads ()
  "Closes any gamepads in lgame.state:*opened-gamepads* that are still connected."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (gamepad-attached? :gamepad v)
               (lgame::sdl-game-controller-close v)))
           lgame.state:*opened-gamepads*)
  (clrhash lgame.state:*opened-gamepads*))

(defun gamepad-name (gamepad)
  (lgame::sdl-game-controller-name gamepad))

(defun gamepad-id-names ()
  "Returns a map of id -> name for the opened controllers in lgame.state:*opened-gamepads*."
  (let ((map (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash k map) (gamepad-name v)))
             lgame.state:*opened-gamepads*)
    map))
