(in-package :cards-sim)

(defun my-lerp (x y amount)
  "Returns the value of linearly interpolating from x to y by the fraction amount"
  (+ x (* amount (- y x))))

(defun lerp-vec2 (vec1 vec2 amount)
  (vec2 (my-lerp (x vec1) (x vec2) amount)
        (my-lerp (y vec1) (y vec2) amount)))

