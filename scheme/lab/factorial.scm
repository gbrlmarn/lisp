(define (factorial x)
	(if (= x 1)
		1
	(* x (factorial (- x 1)))))

(define (factorial2 x)
	(fact-iter 1 1 x))

(define (fact-iter product counter max-count)
	(if (> counter max-count)
		product
		(fact-iter (* counter product)
			(+ counter 1)
			max-count)))


(define (+ a b)
	(if (= a 0)
		b
		(inc (+ (dec a ) b))))

(define (+ a b)
	(if (= a 0)
		b
		(+ (dec a) (inc b))))

; Ackermann function
(define (A x y)
	(cond 
		((= y 0) 0)
  	((= x 0) (* 2 y))
		((= y 1) 2)
		(else 
			(A (- x 1)
			(A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))
