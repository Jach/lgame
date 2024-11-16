(in-package #:lgame.draw)

(defun render-fill-circle (renderer x y radius &optional quadrant)
  "If QUADRANT is given as one of :top-left, :top-right, :bottom-left, or :bottom-right,
   then only that quadrant will be drawn."
  (loop for w from 0 to (* 2 radius) do
        (loop for h from 0 to (* 2 radius) do
              (let ((dx (- radius w))
                    (dy (- radius h)))
                (when (<= (+ (* dx dx) (* dy dy))
                          (* radius radius))
                  (when (case quadrant
                          ((:bottom-right) (and (<= w radius) (<= h radius)))
                          ((:bottom-left) (and (>= w radius) (<= h radius)))
                          ((:top-left) (and (>= w radius) (>= h radius)))
                          ((:top-right) (and (<= w radius) (>= h radius)))
                          (t t))
                    (lgame::sdl-render-draw-point renderer (+ x dx) (+ y dy))))))))

(defun render-rounded-filled-rect (renderer rect radius)
  (multiple-value-bind (x0 y0 w0 h0) (lgame.rect:rect-fields rect)
    (setf radius (truncate (min radius (/ w0 2) (/ h0 2))))
    ; shrunken main rect
    (lgame.rect:with-inflated-rect (shrunk rect (- radius) (- radius))
      (sdl2:render-fill-rect renderer shrunk))

    ; corners
    (render-fill-circle renderer (+ x0 radius) (+ y0 radius) radius)
    (render-fill-circle renderer (+ x0 (1- w0) (- radius)) (+ y0 radius) radius)
    (render-fill-circle renderer (+ x0 radius) (+ y0 (1- h0) (- radius)) radius)
    (render-fill-circle renderer (+ x0 (1- w0) (- radius)) (+ y0 (1- h0) (- radius)) radius)

    ; connecting rects
    ; top
    (with-rect (r (+ x0 radius) y0 (- w0 (* radius 2)) radius)
      (sdl2:render-fill-rect renderer r))
    ; bottom
    (with-rect (r (+ x0 radius) (+ y0 h0 (- radius)) (- w0 (* radius 2)) radius)
      (sdl2:render-fill-rect renderer r))
    ; left
    (with-rect (r x0 (+ y0 radius) radius (- h0 (* radius 2)))
      (sdl2:render-fill-rect renderer r))
    ; right
    (with-rect (r (+ x0 w0 (- radius)) (+ y0 radius) radius (- h0 (* radius 2)))
      (sdl2:render-fill-rect renderer r))))
