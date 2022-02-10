#|
From https://www.pygame.org/docs/tut/PygameIntro.html
WARNING: You should not write your game code at all like this (that is,
top-level execution). It is included here merely for illustration.

Here is the pygame code with annotated # Section Numbers followed by our lisp
code with similar annotations.

import sys, pygame                                                            #1
pygame.init()                                                                 #2

size = width, height = 320, 240                                               #3
speed = [2, 2]
black = 0, 0, 0

screen = pygame.display.set_mode(size)                                        #4

ball = pygame.image.load("intro_ball.gif")                                    #5
ballrect = ball.get_rect()

while 1:                                                                      #6
    for event in pygame.event.get():                                          #7
        if event.type == pygame.QUIT: sys.exit()

    ballrect = ballrect.move(speed)                                           #8
    if ballrect.left < 0 or ballrect.right > width:
        speed[0] = -speed[0]
    if ballrect.top < 0 or ballrect.bottom > height:
        speed[1] = -speed[1]

    screen.fill(black)                                                        #9
    screen.blit(ball, ballrect)
    pygame.display.flip()
|#

(ql:quickload :lgame)                                                         ;1

(defpackage #:taste
  (:use #:cl)
  (:import-from #:lgame.rect #:rect-coord))
(in-package #:taste)

(lgame:init)                                                                  ;2

(defparameter *width* 320)                                                    ;3
(defparameter *height* 240)
(defparameter *speed* (vector 2 2))
(defparameter *black* '(0 0 0))

(lgame.display:create-window "Taste of lgame" *width* *height*)               ;4
(lgame.display:create-renderer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-dir* (directory-namestring
                         (or *compile-file-pathname* *load-truename*))))

(defparameter *ball* (lgame.loader:load-texture                               ;5
                       (merge-pathnames "intro_ball.png" *source-dir*)))
(defparameter *ball-rect* (lgame.rect:get-texture-rect *ball*))

(loop :named game-loop do                                                     ;6
  (lgame.event:do-event (event)                                               ;7
    (when (= (lgame.event:event-type event) lgame::+sdl-quit+)
      (sdl2:free-rect *ball-rect*)
      (lgame:quit)
      (return-from game-loop)))

  (lgame.rect:move-rect *ball-rect* (aref *speed* 0) (aref *speed* 1))        ;8
  (if (or (< (rect-coord *ball-rect* :left) 0)
          (> (rect-coord *ball-rect* :right) *width*))
      (setf (aref *speed* 0) (- (aref *speed* 0))))
  (if (or (< (rect-coord *ball-rect* :top) 0)
          (> (rect-coord *ball-rect* :bottom) *height*))
      (setf (aref *speed* 1) (- (aref *speed* 1))))

  ;(lgame.display:fill *black*)
  (lgame::sdl-set-render-draw-color lgame:*renderer* 0 0 0 255)               ;9
  (lgame::sdl-render-clear lgame:*renderer*)

  (lgame::sdl-render-copy lgame:*renderer* *ball* nil *ball-rect*)
  (lgame::sdl-render-present lgame:*renderer*))


#|
Section Commentary:

1: Use of quickload isn't needed in a real project using asdf. Defining and
entering a package technically isn't needed either, but would be unacceptably
bad practice to leave the example without them...

2: Same as pygame

3: Slightly less economical for defining. Several choices for data structures
here, I picked a vector for *speed* though a (list 2 2) would have worked too,
however that leads to the temptation of a literal list '(2 2) where such
literals should be used immutably, which *speed* here isn't, but *black* is.

4: Need two function calls instead of 1 due to SDL2 giving both a window and a
renderer. I may one day create a unifying create-screen command that does both,
but for now being explicit about certain SDL2 features is better.

5: Uses lgame.loader:load-texture. Could be written just the same as pygame,

(defparameter *ball* (lgame.loader:load-texture "intro_ball.png"))

however that would limit the script to only working when executed from its
directory (like pygame). I opted for more robustness here by using this file's
directory as the source directory for the ball asset. Using the eval-when
wrapper was done to support the further possibility of compile-and-load from an
editor or REPL that has placed the .fasl file into some other location than next
to the .lisp file.

6: Infinite game loop. I opted to give it a name to break out of from the inner
event loop that follows, rather than call quit.

7: Mostly the same event loop, other than being more 'proper' in breaking out
instead of hard quitting, and making sure to free the rect and quit lgame.

8: Movement is done in-place so a reassignment doesn't need to happen.
Checking for collision is uglier.

9: todo

|#
