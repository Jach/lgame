(ql:quickload "lgame")
(ql:quickload "pettomato-deque")

(defvar sw 80)
(defvar sh 24)

(defvar snake-x (/ sw 4))
(defvar snake-y (/ sh 2))

(defvar snake (make-instance 'pettomato-deque:deque))
(pettomato-deque:push-back snake (list snake-x snake-y))
(pettomato-deque:push-back snake (list (- snake-x 1) snake-y))
(pettomato-deque:push-back snake (list (- snake-x 2) snake-y))

(defvar dir lgame::+sdl-scancode-right+)

(defvar food (list (/ sw 2) (/ sh 2)))

(defvar running? nil)

;3 mins 30s left, should be able to do better

(defun main ()
  (lgame:init)
  (lgame.display:create-centered-window "snake" 800 600)
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size sw sh)

  (setf running? t)
  (lgame.time:clock-start)

  (unwind-protect
    (loop while running? do
          (game-tick))
    (lgame:quit)))

;2 mins 40s left

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (= (lgame.event:event-type event) lgame::+sdl-quit+)
      (setf running? nil))
    (when (= (lgame.event:event-type event) lgame::+sdl-keydown+)
      (setf dir (lgame.event:key-scancode event))))

  (let ((new-head (copy-list (pettomato-deque:peek-front snake))))
    (when (= dir lgame::+sdl-scancode-left+)
      (decf (first new-head)))
    (when (= dir lgame::+sdl-scancode-right+)
      (incf (first new-head)))
    (when (= dir lgame::+sdl-scancode-up+)
      (decf (second new-head)))
    (when (= dir lgame::+sdl-scancode-down+)
      (incf (second new-head)))

    (pettomato-deque:push-front snake new-head)

    (if (equal new-head food)
        (setf food (loop with snake = (pettomato-deque:deque->list snake)
                         for new-food = (list (1+ (random (1- sw)))
                                              (1+ (random (1- sh))))
                         unless (member new-food snake :test #'equal) ; time!!!!
                         return new-food))
        (pettomato-deque:pop-back snake))

    (lgame.render:clear)
    (lgame.render:with-draw-color (0 80 0)
      (pettomato-deque:do-all-elements (seg snake)
        (lgame::sdl-render-draw-point lgame:*renderer* (first seg) (second seg))))
    (lgame.render:with-draw-color (200 0 0)
      (lgame::sdl-render-draw-point lgame:*renderer* (first food) (second food)))
    (lgame.render:present)
    (lgame.time:clock-tick 10)))

(main) ; 1 minute 30 seconds over
