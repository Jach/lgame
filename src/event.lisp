(in-package #:lgame.event)

(annot:enable-annot-syntax)

@export
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

@export
(defmacro do-event ((event) &body loop-body)
  "Helper macro to iterate through SDL's event list until it is empty,
   binding each SDL_Event to event."
  `(with-event (,event)
     (loop until (zerop (sdl2-ffi.functions:sdl-poll-event ,event))
           do
           ,@loop-body)))

@export
(defmacro ref (event &rest fields)
  `(plus-c:c-ref ,event sdl2-ffi:sdl-event ,@fields))

@export
(defun event-type (event)
  (ref event :type))

@export
(defun key-scancode (event)
  ;(plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :scancode))
  (ref event :key :keysym :scancode))

@export
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

@export
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

