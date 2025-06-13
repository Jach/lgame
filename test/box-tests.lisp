(defpackage #:lgame.test.box-tests
  (:use #:cl #:fiveam #:lgame.box)
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
    (is (= 10 (box-min-x b1)))
    (is (= 20 (box-min-y b1)))
    (is (= 40 (box-max-x b1))) ; 10 + 30
    (is (= 60 (box-max-y b1))) ; 20 + 40

    (is (= 10 (box-x b1)))
    (is (= 20 (box-y b1)))
    (is (= 30 (box-width b1)))
    (is (= 40 (box-height b1)))

    (is (= 5 (box-min-x b2)))
    (is (= 15 (box-min-y b2)))
    (is (= 55 (box-max-x b2)))
    (is (= 115 (box-max-y b2)))
    (is (= 50 (box-width b2)))
    (is (= 100 (box-height b2)))))

(test box-creation-with-floats
  (let ((b (make-box 10.5 20.25 30.5 40.75)))
    (is (= 10.5 (box-min-x b)))
    (is (= 20.25 (box-min-y b)))
    (is (approx= 41.0 (box-max-x b)))    ; 10.5 + 30.5
    (is (approx= 61.0 (box-max-y b)))    ; 20.25 + 40.75
    (is (approx= 30.5 (box-width b)))
    (is (approx= 40.75 (box-height b)))))

(test box-copy
  (let* ((b1 (make-box 10 20 30 40))
         (b2 (copy-box b1)))
    (is (not (eq b1 b2))) ; Should be different objects
    (is (= (box-min-x b1) (box-min-x b2)))
    (is (= (box-min-y b1) (box-min-y b2)))
    (is (= (box-max-x b1) (box-max-x b2)))
    (is (= (box-max-y b1) (box-max-y b2)))))

;;; Tests for (setf box-x/y/width/height)
(test box-setf-basic-accessors
  (let ((b (make-box 0 0 10 20)))
    (setf (box-x b) 5)
    (is (= 5 (box-min-x b)))
    (is (= 15 (box-max-x b))) ; width 10 preserved
    (is (= 10 (box-width b)))

    (setf (box-y b) 7)
    (is (= 7 (box-min-y b)))
    (is (= 27 (box-max-y b))) ; height 20 preserved
    (is (= 20 (box-height b)))

    (setf (box-width b) 100)
    (is (= 5 (box-min-x b)))   ; min-x preserved
    (is (= 105 (box-max-x b)))
    (is (= 100 (box-width b)))

    (setf (box-height b) 200)
    (is (= 7 (box-min-y b)))   ; min-y preserved
    (is (= 207 (box-max-y b)))
    (is (= 200 (box-height b)))))

;;; Tests for box-attr getter
(test box-attr-getters
  (let ((b (make-box 10 20 30 40))) ; min-x=10, min-y=20, max-x=40, max-y=60
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
    (is (= 110 (box-max-x b)))))

(test box-attr-setf-right
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :right) 50)
    (is (= 50 (box-max-x b)))
    (is (= 100 (box-width b)) "width preserved")
    (is (= -50 (box-x b)))))

(test box-attr-setf-top
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :top) 20)
    (is (= 20 (box-y b)))
    (is (= 200 (box-height b))) ; height preserved
    (is (= 220 (box-max-y b)))))

(test box-attr-setf-bottom
  (let ((b (make-box 0 0 100 200)))
    (setf (box-attr b :bottom) 60)
    (is (= 60 (box-max-y b)))
    (is (= 200 (box-height b))) ; height preserved
    (is (= -140 (box-y b)))))

(test box-attr-setf-centerx
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :centerx) 10)
    (is (= 5 (box-min-x b)))   ; 10 - 10/2
    (is (= 15 (box-max-x b)))  ; 10 + 10/2
    (is (= 10 (box-width b))))) ; width preserved

(test box-attr-setf-centery
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :centery) 15)
    (is (= 5 (box-min-y b)))   ; 15 - 20/2
    (is (= 25 (box-max-y b)))  ; 15 + 20/2
    (is (= 20 (box-height b))))) ; height preserved

(test box-attr-setf-center
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :center) '(30 40))
    (is (= 25 (box-min-x b)))  ; 30 - 10/2
    (is (= 35 (box-max-x b)))  ; 30 + 10/2
    (is (= 30 (box-min-y b)))  ; 40 - 20/2
    (is (= 50 (box-max-y b)))  ; 40 + 20/2
    (is (= 10 (box-width b)))
    (is (= 20 (box-height b)))))

(test box-attr-setf-topleft
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :topleft) '(50 60))
    (is (= 50 (box-x b)))
    (is (= 60 (box-y b)))
    (is (= 10 (box-width b)))   ; width/height preserved
    (is (= 20 (box-height b)))
    (is (= 60 (box-max-x b)))
    (is (= 80 (box-max-y b)))))

(test box-attr-setf-bottomleft
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :bottomleft) '(5 25))
    (is (= 5 (box-x b)))        ; left is 5
    (is (= 25 (box-max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-y b)))        ; 25 - 20
    (is (= 15 (box-max-x b))))) ; 5 + 10

(test box-attr-setf-topright
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :topright) '(15 5))
    (is (= 15 (box-max-x b)))   ; right is 15
    (is (= 5 (box-y b)))        ; top is 5
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; 15 - 10
    (is (= 25 (box-max-y b))))) ; 5 + 20

(test box-attr-setf-bottomright
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :bottomright) '(15 25))
    (is (= 15 (box-max-x b)))   ; right is 15
    (is (= 25 (box-max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; 15 - 10
    (is (= 5 (box-y b)))))      ; 25 - 20

(test box-attr-setf-midtop
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :midtop) '(10 5))
    (is (= 10 (box-attr b :centerx)))
    (is (= 5 (box-y b)))        ; top is 5
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; centerx 10 - width/2 (5)
    (is (= 15 (box-max-x b)))   ; centerx 10 + width/2 (5)
    (is (= 25 (box-max-y b))))) ; top 5 + height 20

(test box-attr-setf-midleft
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :midleft) '(5 15))
    (is (= 5 (box-x b)))        ; left is 5
    (is (= 15 (box-attr b :centery)))
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-y b)))        ; centery 15 - height/2 (10)
    (is (= 25 (box-max-y b)))   ; centery 15 + height/2 (10)
    (is (= 15 (box-max-x b))))) ; left 5 + width 10

(test box-attr-setf-midbottom
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :midbottom) '(10 25))
    (is (= 10 (box-attr b :centerx)))
    (is (= 25 (box-max-y b)))   ; bottom is 25
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; centerx 10 - width/2 (5)
    (is (= 15 (box-max-x b)))   ; centerx 10 + width/2 (5)
    (is (= 5 (box-y b)))))      ; bottom 25 - height 20

(test box-attr-setf-midright
  (let ((b (make-box 0 0 10 20))) ; min-x=0, min-y=0, max-x=10, max-y=20
    (setf (box-attr b :midright) '(15 15))
    (is (= 15 (box-max-x b)))   ; right is 15
    (is (= 15 (box-attr b :centery)))
    (is (= 10 (box-width b)))   ; width preserved
    (is (= 20 (box-height b)))  ; height preserved
    (is (= 5 (box-x b)))        ; right 15 - width 10
    (is (= 5 (box-y b)))        ; centery 15 - height/2 (10)
    (is (= 25 (box-max-y b))))) ; centery 15 + height/2 (10)

(test box-attr-setf-size
  (let ((b (make-box 10 20 0 0)))
    (setf (box-attr b :size) '(70 80))
    (is (= 10 (box-x b)))        ; x/y preserved
    (is (= 20 (box-y b)))
    (is (= 70 (box-width b)))
    (is (= 80 (box-height b)))
    (is (= 80 (box-max-x b)))  ; 10 + 70
    (is (= 100 (box-max-y b))))) ; 20 + 80

(test box-attr-setf-min-max-direct
  (let ((b (make-box 0 0 10 20)))
    (setf (box-attr b :min-x) 5)
    (is (= 5 (box-min-x b)))
    (setf (box-attr b :max-x) 25)
    (is (= 25 (box-max-x b)))
    (is (= 20 (box-width b))) ; width changed as expected

    (setf (box-attr b :min-y) 7)
    (is (= 7 (box-min-y b)))
    (setf (box-attr b :max-y) 27)
    (is (= 27 (box-max-y b)))
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
    (is (= 10 (box-min-x b)))
    (is (= 20 (box-min-y b)))
    (is (= 40 (box-max-x b)))
    (is (= 60 (box-max-y b)))))

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
  (is-false (box-contains-point? b1 10 5))  ; x == max-x
  (is-false (box-contains-point? b1 5 10))  ; y == max-y
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
                               (write-to-string b)))))

