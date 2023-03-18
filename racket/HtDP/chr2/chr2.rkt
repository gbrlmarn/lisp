(require 2htdp/image)

(circle 10 "solid" "red")

;; Exercise 11. Define a function that consumes two numbers, x and y, and that computes the distance of point (x, y) to the origin. In exercise 1 you developed the right-hand side of this function concrete values of x and y. Now add a header.
(define (distance-to-origin x y)
  (sqrt (+ (expt x 2) (expt y 2))))
;; The distance of a point A (x, y) from the origin O (0, 0) is given by OA = \(\sqrt{(x - 0)^{2} + (y - 0)^{2}}\) 

;; Exercise 12. Define the function cvolume, which accepts the length of a side of an equilateral cube and computes its volume. If you have time, consider defining csurface, too.
(define (cvolume side-length)
  (* side-length side-length side-length))
(cvolume 3)

;; Exercise 13. Define the function string-first, which extracts the last 1String from a non-empty string.
(define (string-first str)
  (if (null? str)
      (error "String is null..." str)
      (car (string->list str))))
(string-first "hello")

;; Exercise 14. Define the function string-last, which extracts the last 1String from a non-empty stirng.
(define (string-last str)
  (if (null? str)
      (error "String is null..." str)
      (car (reverse (string->list str)))))
(string-last "hello")

;; Exercise 15. Define ==>. The functions consumes two Boolean values, call them sunny and friday. Its answer is #true if sunny is false or friday is true.
(define (==> sunny friday)
  (or (not sunny) friday))
(==> true true)

;; Exercise 16. Define the function image-area, which counts the number of pixels in a given image. See exercise 6 for ideas.
(require 2htdp/image)
(define test-cube
  (rectangle 20 20 "solid" "white"))
(define (image-area image)
  (* (image-height image)
     (image-width image)))
(image-area test-cube)

;; Exercise 17. Define the function image-classify, which consumes an image and conditionaly produces "tall" if the image is taller than wide, "wide" if it is wider than tall, or "square" if its width and height are the same.
(define (image-classify image)
  (cond ((> (image-height image) (image-width image))
	 "tall")
	((< (image-height image) (image-width image))
	 "wide")
	((= (image-height image) (image-width image))
	 "square")
	(else (error "Cannot classify the image" image))))
(image-classify test-cube)

;; Exercise 18. Define the function string-join, which consumes two string and appends them with "_" in between. See exercise 2 for ideas.
(define (string-join str1 str2)
  (string-append str1 "_" str2))
(string-join "miky" "tauru")


;; Exercise 19. Define the function string-insert, which consumes a string str plus a number i and inserts "_" at the ith position of str. Assume i is a number between 0 and the length of a given string(inclusive).
(define (string-insert str i)
  ()
  
