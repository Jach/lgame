(defpackage #:lgame.test.box-tests
  (:use #:cl #:fiveam #:lgame.box)
  (:import-from #:lgame.box
                 #:.min-x
                 #:.min-y
                 #:.max-x
                 #:.max-y)
  (:export #:run-box-tests))
(in-package #:lgame.test.box-tests)

;(setf *run-test-when-defined* 'T)

(def-suite box-suite
  :description "Tests for the lgame.box system.")

(defun run-box-tests ()
  (run! 'box-suite))

(defun approx= (a b &optional (epsilon 0.00001))
   (< (abs (- a b)) epsilon))

(in-suite box-suite)

;;; Tests for Constructors and Basic Accessors
(test box-creation-and-basic-accessors
  (let ((b1 (make-box 10 20 30 40))
        (b2 (make-box-from-minmax 5 15 55 115)))
    (is (= 10 (.min-x b1)))
    (is (= 20 (.min-y b1)))
    (is (= 40 (.max-x b1))) ; 10 + 30
    (is (= 60 (.max-y b1))) ; 20 + 40

    (is (= 10 (box-x b1)))
    (is (= 20 (box-y b1)))
    (is (= 30 (box-width b1)))
    (is (= 40 (box-height b1)))

    (is (= 5 (.min-x b2)))
    (is (= 15 (.min-y b2)))
    (is (= 55 (.max-x b2)))
    (is (= 115 (.max-y b2)))
    (is (= 50 (box-width b2)))
    (is (= 100 (box-height b2)))))

(test box-creation-with-floats
  (let ((b (make-box 10.5 20.25 30.5 40.75)))
    (is (= 10.5 (.min-x b)))
    (is (= 20.25 (.min-y b)))
    (is (approx= 41.0 (.max-x b)))    ; 10.5 + 30.5
    (is (approx= 61.0 (.max-y b)))    ; 20.25 + 40.75
    (is (approx= 30.5 (box-width b)))
    (is (approx= 40.75 (box-height b)))))

(test box-copy
  (let* ((b1 (make-box 10 20 30 40))
         (b2 (copy-box b1)))
    (is (not (eq b1 b2))) ; Should be different objects
    (is (= (.min-x b1) (.min-x b2)))
    (is (= (.min-y b1) (.min-y b2)))
    (is (= (.max-x b1) (.max-x b2)))
    (is (= (.max-y b1) (.max-y b2)))))

;;; Tests for (setf box-x/y/width/height)
(test box-setf-basic-accessors
  (let ((b (make-box 0 0 10 20)))
    (setf (box-x b) 5)
    (is (= 5 (.min-x b)))
    (is (= 15 (.max-x b))) ; width 10 preserved
    (is (= 10 (box-width b)))

    (setf (box-y b) 7)
    (is (= 7 (.min-y b)))
    (is (= 27 (.max-y b))) ; height 20 preserved
    (is (= 20 (box-height b)))

    (setf (box-width b) 100)
    (is (= 5 (.min-x b)))   ; .min-x preserved
    (is (= 105 (.max-x b)))
    (is (= 100 (box-width b)))

    (setf (box-height b) 200)
    (is (= 7 (.min-y b)))   ; .min-y preserved
    (is (= 207 (.max-y b)))
    (is (= 200 (box-height b)))))

;;; Tests for box-attr getter
(test box-attr-getters
  (let ((b (make-box 10 20 30 40))) ; .min-x=10, .min-y=20, .max-x=40, .max-y=60
    (is (= 10 (box-attr b :x)))
    (is (= 20 (box-attr b :y)))
    (is (= 30 (box-attr b :width)))
    (is (= 40 (box-attr b :height)))
    (is (= 10 (box-attr b :min-x)))
    (is (= 20 (box-attr b :min-y)))
    (is (= 40 (box-attr b :max-x)))
    (is (= 60 (box-attr b :max-y)))
    (is (= 10 (box-attr b :left)))
    (is (= 20 (box-attr b :top)))
    (is (= 40 (box-attr b :right)))
    (is (= 60 (box-attr b :bottom)))
    (is (= 25 (box-attr b :centerx))) ; 10 + 30/2
    (is (= 40 (box-attr b :centery))) ; 20 + 40/2
    (is (equal '(10 20) (box-attr b :topleft)))
    (is (equal '(10 60) (box-attr b :bottomleft)))
    (is (equal '(40 20) (box-attr b :topright)))
    (is (equal '(40 60) (box-attr b :bottomright)))
    (is (equal '(25 20) (box-attr b :midtop)))
    (is (equal '(10 40) (box-attr b :midleft)))
    (is (equal '(25 60) (box-attr b :midbottom)))
    (is (equal '(40 40) (box-attr b :midright)))
    (is (equal '(25 40) (box-attr b :center)))
    (is (equal '(30 40) (box-attr b :size)))))

;;; Tests for (setf box-attr) - comprehensive
(test box-attr-setf-left
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :left) 10)
    (is (= 10 (box-x b)))
    (is (= 100 (box-width b))) ; width preserved
    (is (= 110 (.max-x b)))))

(test box-attr-setf-right
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :right) 50)
    (is (= 50 (.max-x b)))
    (is (= 100 (box-width b)) "width preserved")
    (is (= -50 (box-x b)))))

(test box-attr-setf-top
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :top) 20)
    (is (= 20 (box-y b)))
    (is (= 200 (box-height b))) ; height preserved
    (is (= 220 (.max-y b)))))

(test box-attr-setf-bottom
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :bottom) 60)
    (is (= 60 (.max-y b)))
    (is (= 200 (box-height b))) ; height preserved
    (is (= -140 (box-y b)))))

(test box-attr-setf-centerx
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :centerx) 10)
    (is (= 5 (.min-x b)))   ; 10 - 10/2
    (is (= 15 (.max-x b)))  ; 10 + 10/2
    (is (= 10 (box-width b))))) ; width preserved

(test box-attr-setf-centery
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :centery) 15)
    (is (= 5 (.min-y b)))   ; 15 - 20/2
    (is (= 25 (.max-y b)))  ; 15 + 20/2
    (is (= 20 (box-height b))))) ; height preserved

(test box-attr-setf-center
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :center) '(30 40))
    (is (= 25 (.min-x b)))  ; 30 - 10/2
    (is (= 35 (.max-x b)))  ; 30 + 10/2
    (is (= 30 (.min-y b)))  ; 40 - 20/2
    (is (= 50 (.max-y b)))  ; 40 + 20/2
    (is (= 10 (box-width b)))
    (is (= 20 (box-height b)))))

(test box-attr-setf-topleft
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :topleft) '(50 60))
    (is (= 50 (box-x b)))
    (is (= 60 (box-y b)))
    (is (= 10 (box-width b)))   ; width/height preserved
    (is (= 20 (box-height b)))
    (is (= 60 (.max-x b)))
    (is (= 80 (.max-y b)))))

(test box-attr-setf-bottomleft
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :bottomleft) '(5 25))
    (is (= 5 (box-x b)))        ; left is 5
    (is (= 25 (.max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-y b)))        ; 25 - 20
    (is (= 15 (.max-x b))))) ; 5 + 10

(test box-attr-setf-topright
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :topright) '(15 5))
    (is (= 15 (.max-x b)))   ; right is 15
    (is (= 5 (box-y b)))        ; top is 5
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; 15 - 10
    (is (= 25 (.max-y b))))) ; 5 + 20

(test box-attr-setf-bottomright
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :bottomright) '(15 25))
    (is (= 15 (.max-x b)))   ; right is 15
    (is (= 25 (.max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; 15 - 10
    (is (= 5 (box-y b)))))      ; 25 - 20

(test box-attr-setf-midtop
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :midtop) '(10 5))
    (is (= 10 (box-attr b :centerx)))
    (is (= 5 (box-y b)))        ; top is 5
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; centerx 10 - width/2 (5)
    (is (= 15 (.max-x b)))   ; centerx 10 + width/2 (5)
    (is (= 25 (.max-y b))))) ; top 5 + height 20

(test box-attr-setf-midleft
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :midleft) '(5 15))
    (is (= 5 (box-x b)))        ; left is 5
    (is (= 15 (box-attr b :centery)))
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-y b)))        ; centery 15 - height/2 (10)
    (is (= 25 (.max-y b)))   ; centery 15 + height/2 (10)
    (is (= 15 (.max-x b))))) ; left 5 + width 10

(test box-attr-setf-midbottom
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :midbottom) '(10 25))
    (is (= 10 (box-attr b :centerx)))
    (is (= 25 (.max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; centerx 10 - width/2 (5)
    (is (= 15 (.max-x b)))   ; centerx 10 + width/2 (5)
    (is (= 5 (box-y b)))))      ; bottom 25 - height 20

(test box-attr-setf-midright
  (let ((b (make-box 0 0 10 20))) ; .min-x=0, .min-y=0, .max-x=10, .max-y=20
    (setf (box-attr b :midright) '(15 15))
    (is (= 15 (.max-x b)))   ; right is 15
    (is (= 15 (box-attr b :centery)))
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; right 15 - width 10
    (is (= 5 (box-y b)))        ; centery 15 - height/2 (10)
    (is (= 25 (.max-y b))))) ; centery 15 + height/2 (10)

(test box-attr-setf-size
  (let ((b (make-box 10 20 0 0)))
    (setf (box-attr b :size) '(70 80))
    (is (= 10 (box-x b)))        ; x/y preserved
    (is (= 20 (box-y b)))
    (is (= 70 (box-width b)))
    (is (= 80 (box-height b)))
    (is (= 80 (.max-x b)))  ; 10 + 70
    (is (= 100 (.max-y b))))) ; 20 + 80

(test box-attr-setf-min-max-direct
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :min-x) 5)
    (is (= 5 (.min-x b)))
    (setf (box-attr b :max-x) 25)
    (is (= 25 (.max-x b)))
    (is (= 20 (box-width b))) ; width changed as expected

    (setf (box-attr b :min-y) 7)
    (is (= 7 (.min-y b)))
    (setf (box-attr b :max-y) 27)
    (is (= 27 (.max-y b)))
    (is (= 20 (box-height b))))) ; height changed as expected

(test box-attr-setf-x-y-width-height-direct
  (let ((b (make-box 0 0 0 0)))
    (setf (box-attr b :x) 10)
    (setf (box-attr b :y) 20)
    (setf (box-attr b :width) 30)
    (setf (box-attr b :height) 40)
    (is (= 10 (box-x b)))
    (is (= 20 (box-y b)))
    (is (= 30 (box-width b)))
    (is (= 40 (box-height b)))
    (is (= 10 (.min-x b)))
    (is (= 20 (.min-y b)))
    (is (= 40 (.max-x b)))
    (is (= 60 (.max-y b)))))

;;; Tests for Collision and Containment
(test box-collision-and-containment
  (let ((b1 (make-box 0 0 10 10))     ; 0,0 to 10,10
        (b2 (make-box 5 5 10 10))     ; 5,5 to 15,15
        (b3 (make-box 20 20 5 5))   ; 20,20 to 25,25
        (b4 (make-box 8 8 4 4))     ; 8,8 to 12,12 (b1 contains b4, b2 contains b4)
        (b5 (make-box 0 0 2 2)))     ; miniscule box at origin

  ;; boxes-intersect? / boxes-collide?
  (is-true (boxes-intersect? b1 b2))
  (is-true (boxes-collide? b1 b2))
  (is-false (boxes-intersect? b1 b3))
  (is-false (boxes-collide? b1 b3))
  (is-true (boxes-intersect? b1 b4))
  (is-true (boxes-intersect? b2 b4))
  (is-true (boxes-intersect? b1 b1)) ; Self intersection
  (is-true (boxes-intersect? b1 b5))

  ;; Touching edges should intersect
  (let ((touching-left (make-box -10 0 10 10))) ; -10,0 to 0,10
    (is-true (boxes-intersect? b1 touching-left)))
  (let ((touching-right (make-box 10 0 10 10))) ; 10,0 to 20,10
    (is-true (boxes-intersect? b1 touching-right)))

  ;; Separated by one pixel (or unit)
  (let ((separate-left (make-box -11 0 10 10)))
    (is-false (boxes-intersect? b1 separate-left)))

  ;; box-contains-point? (max edge is exclusive)
  (is-true (box-contains-point? b1 0 0))
  (is-true (box-contains-point? b1 5 5))
  (is-true (box-contains-point? b1 9.99 9.99))
  (is-false (box-contains-point? b1 10 5))  ; x == .max-x
  (is-false (box-contains-point? b1 5 10))  ; y == .max-y
  (is-false (box-contains-point? b1 10 10))
  (is-false (box-contains-point? b1 -1 5))
  (is-false (box-contains-point? b1 5 -1))
  (is-false (box-contains-point? b1 11 5))
  (is-false (box-contains-point? b1 5 11))

  ;; Test with floating point containment
  (let ((bf (make-box 0.0 0.0 1.0 1.0)))
    (is-true (box-contains-point? bf 0.5 0.5))
    (is-true (box-contains-point? bf 0.0 0.0))
    (is-false (box-contains-point? bf 1.0 0.5)))))


;;; Tests for with-box-as-sdl-rect macro (needs SDL2 FFI to be available)
;;; We can't fully test the SDL calls here without an SDL context,
;;; but we can test if the values are set correctly on the foreign pointer.
(defparameter *sdl2-ffi-present* (and (find-package :sdl2)
                                      (fboundp (intern "RECT-X" :sdl2)))
  "Check if essential SDL2 FFI parts are loaded for this test.")

(test with-box-as-sdl-rect-macro
  (if *sdl2-ffi-present*
      (let ((b-int (make-box 10 20 30 40))
            (b-float (make-box 10.2 20.8 30.5 40.4))
            (b-float-near-half (make-box 5.5 6.5 7.5 8.5)))
        (with-box-as-sdl-rect (r b-int)
          (is (= 10 (sdl2:rect-x r)))
          (is (= 20 (sdl2:rect-y r)))
          (is (= 30 (sdl2:rect-width r)))
          (is (= 40 (sdl2:rect-height r))))

        (with-box-as-sdl-rect (r b-float)
          (is (= 10 (sdl2:rect-x r))) ; round(10.2)
          (is (= 21 (sdl2:rect-y r))) ; round(20.8)
          (is (= 30 (sdl2:rect-width r))) ; round(30.5) -> 30, actually
          (is (= 40 (sdl2:rect-height r)))) ; round(40.4)

        (with-box-as-sdl-rect (r b-float-near-half)
          (is (= (round 5.5) (sdl2:rect-x r))) ; 6 (assuming round half to even, or up)
          (is (= (round 6.5) (sdl2:rect-y r))) ; 6 or 7
          (is (= (round 7.5) (sdl2:rect-width r))) ; 8 or 7
          (is (= (round 8.5) (sdl2:rect-height r))))) ; 8 or 9

      (skip "SDL2-FFI not fully available for with-box-as-sdl-rect test.")))

(test print-output
  (let ((b (make-box 10 20 30 40)))
    (is-true (str:starts-with? "#<BOX X:10 Y:20 W:30 H:40 MAX-X:40 MAX-Y:60 "
                               (write-to-string b))
             "output was not as expected -- was ~a" (write-to-string b))))

;;; Tests for Edge Cases and Error Conditions
(test box-zero-dimensions
  (let ((b-zero-width (make-box 10 20 0 30))
        (b-zero-height (make-box 10 20 30 0))
        (b-zero-both (make-box 10 20 0 0)))
    ;; Zero width box
    (is (= 0 (box-width b-zero-width)))
    (is (= 10 (.min-x b-zero-width)))
    (is (= 10 (.max-x b-zero-width))) ; .min-x + 0

    ;; Zero height box
    (is (= 0 (box-height b-zero-height)))
    (is (= 20 (.min-y b-zero-height)))
    (is (= 20 (.max-y b-zero-height))) ; .min-y + 0

    ;; Zero area box
    (is (= 0 (box-width b-zero-both)))
    (is (= 0 (box-height b-zero-both)))))

(test box-negative-dimensions
  ;; Test that negative dimensions are handled
  (signals type-error (make-box 10 20 -5 30))
  (signals type-error (make-box 10 20 30 -10))
  (signals type-error (make-box 10 20 -30 -10)))

(test box-collision-edge-cases
  ;; Test collision detection with zero-area boxes
  (let ((normal-box (make-box 10 10 10 10))    ; 10,10 to 20,20
        (zero-width (make-box 15 12 0 5))      ; 15,12 to 15,17 (line)
        (zero-height (make-box 12 15 5 0))     ; 12,15 to 17,15 (line)
        (zero-area (make-box 15 15 0 0))       ; point at 15,15
        (identical1 (make-box 5 5 10 10))
        (identical2 (make-box 5 5 10 10)))

    ;; Zero-width box intersecting normal box
    (is-true (boxes-intersect? normal-box zero-width) "Normal box should intersect zero-width box inside it")

    ;; Zero-height box intersecting normal box
    (is-true (boxes-intersect? normal-box zero-height) "Normal box should intersect zero-height box inside it")

    ;; Point (zero-area) intersecting normal box
    (is-true (boxes-intersect? normal-box zero-area) "Normal box should intersect point inside it")

    ;; Identical boxes
    (is-true (boxes-intersect? identical1 identical2) "Identical boxes should intersect")

    ;; Point outside normal box
    (let ((point-outside (make-box 25 25 0 0)))
      (is-false (boxes-intersect? normal-box point-outside) "Point outside should not intersect"))

    ;; Zero-area boxes intersecting each other
    (let ((point1 (make-box 5 5 0 0))
          (point2 (make-box 5 5 0 0))
          (point3 (make-box 6 6 0 0)))
      (is-true (boxes-intersect? point1 point2) "Identical points should intersect")
      (is-false (boxes-intersect? point1 point3) "Different points should not intersect"))))

(test box-collision-boundary-precision
  ;; Test precise boundary conditions
  (let ((box1 (make-box 0 0 10 10))     ; 0,0 to 10,10
        (box2 (make-box 10 0 10 10)))   ; 10,0 to 20,10 (touching right edge)

    ;; Boxes touching at edge should intersect (inclusive boundary)
    (is-true (boxes-intersect? box1 box2) "Boxes touching at edge should intersect")

    ;; Test all four edge touching cases
    (let ((touching-top (make-box 0 -10 10 10))     ; 0,-10 to 10,0 (touching top)
          (touching-bottom (make-box 0 10 10 10))   ; 0,10 to 10,20 (touching bottom)
          (touching-left (make-box -10 0 10 10)))   ; -10,0 to 0,10 (touching left)

      (is-true (boxes-intersect? box1 touching-top) "Should intersect when touching top edge")
      (is-true (boxes-intersect? box1 touching-bottom) "Should intersect when touching bottom edge")
      (is-true (boxes-intersect? box1 touching-left) "Should intersect when touching left edge"))

    ;; Boxes separated by tiny amount should not intersect
    (let ((almost-touching (make-box 10.001 0 10 10)))
      (is-false (boxes-intersect? box1 almost-touching) "Boxes separated by tiny gap should not intersect"))))

(test box-large-numbers
  ;; Test with very large coordinate values
  (let ((big-box (make-box 1e6 1e6 1e3 1e3)))
    (is (= 1e6 (box-x big-box)) "Large X coordinate should be preserved")
    (is (= 1e6 (box-y big-box)) "Large Y coordinate should be preserved")
    (is (= 1e3 (box-width big-box)) "Large width should be preserved")
    (is (= 1e3 (box-height big-box)) "Large height should be preserved")
    (is (= (+ 1e6 1e3) (.max-x big-box)) "Max-x calculation should work with large numbers"))

  ;; Test collision detection with large numbers
  (let ((big1 (make-box 1e6 1e6 100 100))
        (big2 (make-box (+ 1e6 50) (+ 1e6 50) 100 100)))
    (is-true (boxes-intersect? big1 big2) "Large boxes should intersect correctly")))

(test box-containment-edge-cases
  (let ((outer (make-box 0 0 100 100)))

    ;; Test point containment at exact boundaries
    (is-true (box-contains-point? outer 0 0) "Should contain point at min corner")
    (is-false (box-contains-point? outer 100 100) "Should NOT contain point at max corner (exclusive)")
    (is-false (box-contains-point? outer 100 0) "Should NOT contain point at max-x boundary")
    (is-false (box-contains-point? outer 0 100) "Should NOT contain point at max-y boundary")
    (is-true (box-contains-point? outer 99.999 99.999) "Should contain point just inside max boundary")

    ;; Test with negative coordinates
    (let ((negative-box (make-box -50 -50 100 100))) ; -50,-50 to 50,50
      (is-true (box-contains-point? negative-box -25 -25) "Should contain point in negative coordinate box")
      (is-true (box-contains-point? negative-box 0 0) "Should contain origin in negative coordinate box")
      (is-false (box-contains-point? negative-box 50 0) "Should not contain point at max boundary"))))

(test box-mixed-numeric-types
  ;; Test mixing integers and floats
  (let ((mixed-box (make-box 10 20.5 30 40.25)))
    (is (= 10 (box-x mixed-box)) "Integer X should be preserved")
    (is (= 20.5 (box-y mixed-box)) "Float Y should be preserved")
    (is (= 30 (box-width mixed-box)) "Integer width should be preserved")
    (is (= 40.25 (box-height mixed-box)) "Float height should be preserved")
    (is (= 40 (.max-x mixed-box)) "Max-x should be sum of int + int")
    (is (= 60.75 (.max-y mixed-box)) "Max-y should be sum of float + float"))

  ;; Test arithmetic operations preserve type appropriately
  (let ((b (make-box 10.5 20.5 5 5)))
    (setf (box-width b) 10.25)
    (is (= 20.75 (.max-x b)) "Mixed arithmetic should work correctly")))

(test box-attr-error-conditions
  ;; Test invalid attribute keys (if they cause errors vs return nil)
  (let ((b (make-box 10 20 30 40)))
    (signals error (box-attr b :invalid-key) "Invalid attribute key should signal error")
    (signals error (box-attr b nil) "Nil attribute key should signal error")
    (signals error (setf (box-attr b :invalid-key) 100) "Setting invalid attribute should signal error")))

(test box-copy-independence
  ;; Test that copied boxes are different
  (let ((b (make-box 1 2 3 4)))
    (is-false (eq b (copy-box b)))))


