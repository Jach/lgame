;; From https://www.pygame.org/docs/tut/PygameIntro.html
;; Equivalent pygame code from the 25 line example is copied in comments,
;; followed by our lisp version.

(defpackage #:taste
  (:use :cl))
(in-package #:taste)

; import pygame
(push (merge-pathnames "../lgame/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload :lgame)

; pygame.init()
(lgame:init)

; size = width, height = 320, 240
; speed = [2, 2]
; black = 0, 0, 0
(defparameter *width* 320)
(defparameter *height* 240)
(defparameter *speed* (list 2 2))

; screen = pygame.display.set_mode(size)
(lgame:create-window "Taste of lgame" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ *width* *height*)
(lgame:create-renderer)

; ball = pygame.image.load("intro_ball.gif")
(defparameter *current-dir* (directory-namestring *load-truename*))
(defparameter *ball* (lgame:load-texture (merge-pathnames "intro_ball.png" *current-dir*)))
; ballrect = ball.get_rect()
(defparameter *ball-rect* (lgame:get-texture-rect *ball*))

; while 1:
(loop
  ; for event in pygame.event.get():
  (lgame:do-event (event)
    ; if event.type == pygame.QUIT: sys.exit()
    (when (= (lgame:event-ref event :type) lgame::+sdl-quit+)
      (uiop:quit))) ;; note we're not doing any memory cleanup as we should
  ; ballrect = ballrect.move(speed)
  (lgame:move-rect *ball-rect* (first *speed*) (second *speed*))
  ; if ballrect.left < 0 or ballrect.right > width:
  (if (or (< (lgame:rect-left *ball-rect*) 0)
          (> (lgame:rect-right *ball-rect*) *width*))
      ; speed[0] = -speed[0]
      (setf (first *speed*) (- (first *speed*))))
  ; if ballrect.top < 0 or ballrect.bottom > height:
  (if (or (< (lgame:rect-top *ball-rect*) 0)
          (> (lgame:rect-bottom *ball-rect*) *height*))
      ; speed[1] = -speed[1]
      (setf (second *speed*) (- (second *speed*))))

  ; screen.fill(black)
  (lgame::sdl-set-render-draw-color lgame:*renderer* 0 0 0 255)
  (lgame::sdl-render-clear lgame:*renderer*)

  ; screen.blit(ball, ballrect)
  (lgame::sdl-render-copy lgame:*renderer* *ball* nil *ball-rect*)
  ; pygame.display.flip()
  (lgame::sdl-render-present lgame:*renderer*))
