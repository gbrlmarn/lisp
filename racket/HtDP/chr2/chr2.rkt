(require 2htdp/image)

(circle 10 "solid" "red")

;; Exercise 11. Define a function that consumes two numbers, x and y, and that computes the distance of point (x, y) to the origin. In exercise 1 you developed the right-hand side of this function concrete values of x and y. Now add a header.
(define (distance-to-origin x y)
  (sqrt (+ (expt x 2) (expt y 2))))
;; The distance of a point A (x, y) from the origin O (0, 0) is given by OA = \(\sqrt{(x - 0)^{2} + (y - 0)^{2}}\) 
