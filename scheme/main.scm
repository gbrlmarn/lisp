; square of the guess
(define (sqr guess)
	(* guess guess))

; improve the guess of the 3rd root of x
(define (improve guess x)
	(/ (+ (/ x (sqr guess)) (* 2 guess)) 3))

; itereate until guess and next guess are equal
(define (good-enough? guess x)
	(= (improve guess x) guess))

; call improve funtion untill good-enough function is satisfied
(define (3rd-iter guess x)
	(if (good-enough? guess x)
		guess
		(3rd-iter (improve guess x) x)))

; 3root of a number 1.1 as a guess to prevent an anomalous result 
(define (3root x)
	(3rd-iter 1.1 x))


(define (fib n)
	(cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

; count change 
(define (count-change amount)
	(cc amount 5))

(define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount (- kinds-of-coins 1))
			(cc (- amount (first-denomination kinds-of-coins))
			kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
	(cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))
		
; exercise 1.11 recursive
(define (f n)
	(cond ((< n 3) n)
		(else (+ (f (- n 1)) 
			(* 2 (f (- n 2)))
			(* 3 (f (- n 3)))))))

; exercise 1.11 iterative
(define (f n)
	(define (f-i a b c count)
		(cond ((< n 3) n)
			((<= count 0) a)
			(else (f-i (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
	(f-i 2 1 0 (- n 2)))

; pascal triangle
(define (pascal-triangle r c)
	(if (or (= r 0) (= c 0))
		1
		(+ (pascal-triangle (- r 1) c) (pascal-triangle r (- c 1)))))

; ex 1.15 sin x ;= x for x --> 0
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x)))) ; x is sin(r/3)
(define (sine angle)
	(if (not (> (abs angle) 0.1))
		angle
		(p (sine (/ angle 3.0)))))

; b^n recursively
(define (expt b n)
	(if (= n 0) 
		1
		(* b (expt b (- n 1)))))

; b^n iterative
(define (expt b n)
	(expt-iter b n 1))

(define (expt-iter b counter product)
	(if (= counter 0) 
		product
		(expt-iter b (- counter 1) ( * b product))))
		
; b^n = (b^(n/2))^2 for n even
; b^n = (b^(n-1))*b for n odd
; recursive version
(define (fast-expt b n)
	(cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (?even n)
	(= (remainder n 2) 0))

; iterative version 1.16
(define (fast-expt b n)
	(expt-iter b n 1))

(define (expt-iter b counter product)
	(cond ((= counter 0) product)
		((even? counter) (expt-iter (square b) (/ counter 2) product))
		(else (expt-iter b (- counter 1) (* b product)))))

; recursive multiply using addition ex 1.17
(define (* a b)
	(if (= b 0)
		0
		(+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
	(cond ((= b 0) 0)
		((even? b) (double (* a (halve b))))
		(else (+ a (* a (- b 1))))))

; iterative multiply using addition ex 1.18
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
	(iter a b 0))
	(define (iter a b accumulator)
		(cond ((= b 0) accumulator)
			((even? b) (iter (double a) (halve b) accumulator))
			(else (iter a (- b 1) (+ accumulator a)))))
	

; logaritmic fibonacci
(define (fib n)
	(fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
	(cond ((= count 0) b) 
		((even? count)
			(fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ count 2)))
		(else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))))
			
; Greatest Common Divisors
; GCD(a, b) = GCD(b, r)
; Euclid's Algorithm
; Recursive Method
(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

; Iterative Method Ex.1.20
; ...


; Prime Numbers
; O(sqrt(n)) order of growth algorithm
(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n) ; this composition gives the numbe of steps.
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
	(= (remainder b a) 0))

; Find if a number is prime
(define (prime? n)
	(= n (smallest-divisor n)))

; O(log(n)) order of growth algorithm
; The Fermat test
; If n is a prime number and a is any positivie integer less than n,
; 	then a^n is congruent to a%n(a modulo n)
(define (expmod base n m)
	(cond ((= n 0) 1)
				((even? n)
					(remainder (square (expmod base (/ n 2) m)) m))
				(else (remainder (* base (expmod base (- n 1) m)) m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
			(else false)))

; Ex 1.22 runtime of the composition
(define (time-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))
(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))
(define (search-for primes lower upper)
	(define (iter n)
		(cond ((<= n upper) (timed-prime-test n) (iter (+ n 2)))))
	(iter (if (odd? lower) lower (+ lower 1))))

; Ex 1.23
; Faster version of smallest-divisor using (next test-divisor) instead of (+ 1 test-divisor)
(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n) ; this composition gives the numbe of steps.
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (next test-divisor))))) 

(define (divides? a b)
	(= (remainder b a) 0))
(define (next n)
	(if (= n 2) 3
		(+ n 2)))

; Ex 1.25 
; The difference is that the first method uses remider after square so we will compute
; shorter numbers. Method 2 computers larger numbers...
(define (expmod base n m)
	(cond ((= n 0) 1)
				((even? n)
					(remainder (square (expmod base (/ n 2) m)) m))
				(else (remainder (* base (expmod base (- n 1) m)) m))))

(define (expmod base n m)
	(remainder (fast-expt base n) m))

; Ex 1.26 - this procedure uses O(n) bacause we need to computer two expmod at each iteration
; 	instead of one.
(define (expmod base n m)
	(cond ((= n 0) 1)
				((even? n)
					(remainder (* (expmod base (/ n 2) m) (expmod base (/ n 2) m)) m))
				(else (remainder (* base (expmod base (-exp 1) m)) m))))

; 1.3 subchapter
(define (sum term a next b) ; term --> function that applies to a next --> step of the sum
	(if (> a b)
		0
		(+ (term a) 
			 (sum term (next a) next b))))

; cube sum
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
	(sum cube a inc b))

; integer sum
(define (identity x) x)
(define (sum-integers a b)
	(sum identity a inc b))

; pi sum
(define (pi-sum a b)
	(define (pi-term x)
		(/ 1 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

; Ex 1.29 Simpson's Rule
; h = (b - a)/n for n even
; yk = f(a + kh) incresing n increses the accuracy
; arguments f a b n --> itegral value
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

(define (simpson f a b n)
	(define h (/ (- b a) n))
	(define (yk k) (* f (+ a (* k h))))
	(define (simpson-term k)
		(* (cond 
				((or (= k 0) (= k n)) 1)
				((odd? k) 4)
				(else 2))
			(yk k)))
	(* (/ h 3) (sum simpson-term 0 inc n)))

; Sum function recursive version 
(define (sum term a next b) ; term --> function that applies to a next --> step of the sum
	(if (> a b)
		0
		(+ (term a) 
			 (sum term (next a) next b))))

; Ex 1.30 - Sum function iterative version
(define (iter-sum term a next b)
	(define (iter a result)
		(if (> a b)
			result	
			(iter (next a) (+ result (term a)))))
	(iter a 0))

; Test for iter sum
(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(iter-sum pi-term a pi-next b))

; Procedures Using Lambda
(define (pi-sum a b)
	(sum (lambda (x) (/ 1.0 (* x (+ x 2))))	
		 a
		 (lambda (x) (+ x 4))
		 b))

(define (plus4 x) (+ x 4))
; Equiv to 
(define plus (lambda (x) (+ x 4)))

; To include local variables we use 'let'
; For f(x, y) = x*a^2 + y*b + a*b, with a = 1 + xy and b = 1 - y -->

; Classical approach
(define (f x y)
	(define (f-helper a b)
		(+ (* x (square a)) (* y b) (* a b)))
	(f-helper (+ 1 (* x y)) (- 1 y)))

;Lambda approach
(define (f x y)
	((lambda (a b)
		(+ (* x (square a)) (* y b) (* a b))) ; Lambda function 
	(+ 1 (* x y)) (- 1 y))) ; Lambda values for a and b

; Let approach
(define (f x y)
	(let ((a (+ 1 (* x y))) ; a and b have those values in
		  (b (- 1 y)))
		  (+ (* x (square a)) (* y b) (* a b)))) ; this expression

; Ex 1.34
(define (f g)
	(g 2))
; for (f f) --> (f 2) --> (2 2)...

; Chapter 2
; Pair is constructed with the command "cons" ex: (define x (cons 1 2))
; car --> prints the first pair element ex: (car x) --> 1 
; cdr --> prints the last  pair element ex: (cdr x) --> 2  

; Represeting Rational Numbers
(define (make-rat n d) 
	(let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
; Display Result
(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))
	
; Ex 2.1 make-rat with negative numbers
(define (make-rat n d)
		(let ((g ((if (< d 0) - +) (abs (gcd (n d))))))
			(cons (/ n g) (/ d g))))



