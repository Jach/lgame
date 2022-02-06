;; From https://www.pygame.org/docs/tut/PygameIntro.html
;; Equivalent pygame code from the 25 line example is copied in comments,
;; followed by our lisp version.

(defpackage #:taste
  (:use :cl))
(in-package #:taste)

; import pygame
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
(lgame.display:create-window "Taste of lgame" lgame::+sdl-windowpos-centered+ lgame::+sdl-windowpos-centered+ *width* *height*)
(lgame.display:create-renderer)

; ball = pygame.image.load("intro_ball.gif")
(defparameter *current-dir* (directory-namestring *load-truename*))
(defparameter *ball* (lgame.loader:load-texture (merge-pathnames "intro_ball.png" *current-dir*)))
; ballrect = ball.get_rect()
(defparameter *ball-rect* (lgame.rect:get-texture-rect *ball*))

; while 1:
(loop
  ; for event in pygame.event.get():
  (lgame.event:do-event (event)
    ; if event.type == pygame.QUIT: sys.exit()
    (when (= (lgame.event:ref event :type) lgame::+sdl-quit+)
      (uiop:quit))) ;; note we're not doing any memory cleanup as we should
  ; ballrect = ballrect.move(speed)
  (lgame.rect:move-rect *ball-rect* (first *speed*) (second *speed*))
  ; if ballrect.left < 0 or ballrect.right > width:
  (if (or (< (lgame.rect:rect-dim *ball-rect* :left) 0)
          (> (lgame.rect:rect-dim *ball-rect* :right) *width*))
      ; speed[0] = -speed[0]
      (setf (first *speed*) (- (first *speed*))))
  ; if ballrect.top < 0 or ballrect.bottom > height:
  (if (or (< (lgame.rect:rect-dim *ball-rect* :top) 0)
          (> (lgame.rect:rect-dim *ball-rect* :bottom) *height*))
      ; speed[1] = -speed[1]
      (setf (second *speed*) (- (second *speed*))))

  ; screen.fill(black)
  ; note that you can use sdl2:render-clear etc. versions here, this just shows the underlying sdl calls are exposed
  (lgame::sdl-set-render-draw-color lgame:*renderer* 0 0 0 255)
  (lgame::sdl-render-clear lgame:*renderer*)

  ; screen.blit(ball, ballrect)
  (lgame::sdl-render-copy lgame:*renderer* *ball* nil *ball-rect*)
  ; pygame.display.flip()
  (lgame::sdl-render-present lgame:*renderer*))
