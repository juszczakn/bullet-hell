(in-package :bullet-hell)

(defun circle-dist (ix iy jx jy)
  (let ((x (abs (- ix jx)))
        (y (abs (- iy jy))))
    (sqrt (+ (* y y) (* x x)))))

(defun circle-collision? (i j)
  "two circle of '(x y rad), returns if collided"
  (destructuring-bind ((ix iy irad) (jx jy jrad)) (list i j)
    (let ((dist (circle-dist ix iy jx jy)))
      (if (> (+ irad jrad) dist)
          t
          nil))))
