#|
Very bad programming style example, but trying for
a short ~5 minutes demo in the spirit
of https://www.youtube.com/watch?v=rbasThWVb-c
See snakeugly.lisp for an even further simplified
version.
|#
(defpackage #:snake
  (:use #:cl))

(ql:quickload "lgame")
(ql:quickload "livesupport")
(ql:quickload "pettomato-deque")

(defparameter *screen-width* 80)
(defparameter *screen-height* 24)

(defparameter *snake-x* (/ *screen-width* 4))
(defparameter *snake-y* (/ *screen-height* 2))

(defparameter *snake* (make-instance 'pettomato-deque:deque))
(pettomato-deque:push-back *snake* (list *snake-x* *snake-y*))
(pettomato-deque:push-back *snake* (list (- *snake-x* 1) *snake-y*))
(pettomato-deque:push-back *snake* (list (- *snake-x* 2) *snake-y*))

(defparameter *direction* lgame::+sdl-scancode-right+)

(defparameter *food* (list (/ *screen-width* 2)
                           (/ *screen-height* 2)))

(defun main ()
  (lgame:init)
  (lgame.display:create-centered-window "Snake" 800 600)
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size *screen-width* *screen-height*)

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))
    (lgame:quit)))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (= (lgame.event:event-type event) lgame::+sdl-quit+)
      (lgame.time:clock-stop))
    (when (= (lgame.event:event-type event) lgame::+sdl-keydown+)
      ;(and (= (lgame.event:event-type event) lgame::+sdl-keydown+)
      ;     (member (lgame.event:key-scancode event)
      ;             (list lgame::+sdl-scancode-left+ lgame::+sdl-scancode-right+
      ;                   lgame::+sdl-scancode-up+ lgame::+sdl-scancode-down+)))
      (setf *direction* (lgame.event:key-scancode event))))

  (let ((new-head (copy-list (pettomato-deque:peek-front *snake*))))
    (when (= *direction* lgame::+sdl-scancode-left+)
      (decf (first new-head)))
    (when (= *direction* lgame::+sdl-scancode-right+)
      (incf (first new-head)))
    (when (= *direction* lgame::+sdl-scancode-up+)
      (decf (second new-head)))
    (when (= *direction* lgame::+sdl-scancode-down+)
      (incf (second new-head)))
    (pettomato-deque:push-front *snake* new-head)

    (if (equal new-head *food*)
        (setf *food* (loop with snake = (pettomato-deque:deque->list *snake*)
                           for new-food = (list (1+ (random (1- *screen-width*)))
                                                (1+ (random (1- *screen-height*))))
                           unless (member new-food snake :test #'equal)
                           return new-food))
        (pettomato-deque:pop-back *snake*)))

  (let ((head (pettomato-deque:peek-front *snake*)))
    ;(when (or (member (first head) (list 0 *screen-width*))
    ;          (member (second head) (list 0 *screen-height*))
    ;          (member head (rest (pettomato-deque:deque->list *snake*)) :test #'equal))
    ;  (lgame.time:clock-stop))
    (when (< (first head) 0)
      (setf (first head) (1- *screen-width*)))
    (when (>= (first head) *screen-width*)
      (setf (first head) 0))
    (when (< (second head) 0)
      (setf (second head) (1- *screen-height*)))
    (when (>= (second head) *screen-height*)
      (setf (second head) 0))
    )

  (lgame.render:clear)

  (lgame.render:with-draw-color (255 255 255)
    (lgame::sdl-render-fill-rect lgame:*renderer* lgame:*screen-rect*))

  (lgame.render:with-draw-color (0 80 0)
    (pettomato-deque:do-all-elements (segment *snake*)
      (lgame::sdl-render-draw-point lgame:*renderer* (first segment) (second segment))))

  (lgame.render:with-draw-color (200 0 0)
    (lgame::sdl-render-draw-point lgame:*renderer* (first *food*) (second *food*))
    (lgame.render:set-draw-color 0 0 0))

  (lgame.render:present)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick 10))

(eval-when (:execute)
  (main))
