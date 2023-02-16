;; Ninety-nine Lisp Problems

(define (my-last lst)
  ;; 1) Find the last box of a list
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))
(my-last '(a b c d))

(define (my-but-last lst)
  ;; 2) Find the last but one box of a list
  (if (null? (cddr lst))
      lst
      (my-but-last (cdr lst))))
(my-but-last '(a b c d))

(define (element-at lst index)
  ;; 3) Find the K'th element of a list
  (if (= index 1)
      (car lst)
      (element-at (cdr lst)
		  (- index 1))))
(element-at '(a b c d) 3)

(define (length lst)
  ;; 4) Find the number of elements of a list
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))
(length '(a b c d))

(define (reverse lst)
  ;; 5) Reverse a list
  (define (iter lst res)
    (if (null? lst)
	res
	(iter (cdr lst)
	      (cons (car lst) res))))
  (iter lst '()))
(reverse '(a b c d))

(define (palindrome lst)
  ;; 6) Find out wheher a list is a palindrome.
  (equal? (reverse lst) lst))
(palindrome '(x a m a x))

(define (my-flatten lst)
  ;; 7) Flatten a nested list structure.
  (if (null? lst)
      '()
      (let ((first (car lst))
	    (rest (cdr lst)))
	(if (pair? first)
	    (append (my-flatten first)
		    (my-flatten rest))
	    (append (list first) 
		    (my-flatten rest))))))

(my-flatten '(a (b (c d) e)))

(define (compress lst)
  ;; 8) Eliminate consecutive duplicates of list elements.
  (cond ((null? lst) '())
	((null? (cdr lst)) lst)
	((equal? (car lst) (cadr lst))
	 (compress (cdr lst)))
	(else
	 (append  (list (car lst))
		  (compress (cdr lst))))))
(compress '(a a a a b c c a a d e e e e))

(define (pack lst)
  ;; 9) Pack consecutive duplicates of list into sublists.
  (if (null? lst)
      '()
      (let iter ((from (cdr lst))
		 (to (list (car lst))))
	(cond
	 ((null? from) (list to))
	 ((equal? (car to)
		  (car from))
	  (iter (cdr from)
		(cons (car from)
		      to)))
	 (else
	  (cons to
		(iter (cdr from)
		      (list (car from)))))))))
(pack '(a a a a b c c a a d e e e e))

(define (encode lst)
  ;; 10) Run-length encoding of a list
  (let iter ((packed (pack lst)))
    (if (null? packed)
	'()
	(cons (list (length (car packed))
		    (car (car packed)))
	 (iter (cdr packed))))))
(encode '(a a a a b c c a a d e e e e))

(define (encode-modified lst)
  ;; 11) Modified run-length encoding
  (let iter ((packed (pack lst)))
    (cond ((null? packed) '())
	  ((= (length (car packed)) 1)
	   (cons (car (car packed))
		 (iter (cdr packed))))
	  (else
	   (cons (list (length (car packed))
		       (car (car packed)))
		 (iter (cdr packed)))))))
(encode-modified '(a a a a b c c a a d e e e e))

(define (decode lst)
  ;; 12) Decode a run-length encoded list.
  (define (constructor times symbol)
    (if (= times 0)
	'()
	(cons symbol (constructor (- times 1) symbol))))
  (define (decode-one encoding)
    (if (pair? encoding)
	(constructor (car encoding) (cadr encoding))
	(list encoding)))
  (if (null? lst)
      '()
      (append (decode-one (car lst))
	      (decode (cdr lst)))))

(decode '((4 A) B (2 C) (2 A) D (4 E)))

(define (encode-direct lst)
  ;; 13) Run-length encoding of a list(direct solution)
  (if (null? lst)
      '()
      (let ((format (lambda (counter elem)
		      (if (= counter 1)
			  elem
			  (list counter elem)))))
	(let iter ((elem (car lst))
		   (rest (cdr lst))
		   (counter 1))
	  (cond
	   ((null? rest)
	    (list (format counter elem)))
	   ((eq? elem (car rest))
	    (iter elem (cdr rest) (+ counter 1)))
	   (else
	    (cons (format counter elem)
		  (iter (car rest)
			(cdr rest)
			1))))))))
(encode-direct '(a a a a b c c a a d e e e e))

(define (dupli lst)
  ;; 14) Duplicate the elements of a list
  (if (null? lst)
      '()
      (append (list (car lst)
		    (car lst))
	      (dupli (cdr lst)))))
(dupli '(a b c c d))

(define (repli lst num)
  ;; 15) Replicate the elements of a list a given number of times
  (if (null? lst)
      '()
      (let iter ((xlst lst)
		 (res '())
		 (count 0))
	(cond
	 ((null? xlst) res)
	 ((= count num)
	  (iter (cdr xlst)
		res
		0))
	 (else
	  (iter xlst
		(append res (list (car xlst)))
		(+ count 1)))))))
(repli '(a b c) 3)

(define (drop lst num)
  ;; 16) Drop every N'th element from a list.
  (let iter ((xlst lst)
	     (count 1))
    (cond ((null? xlst) '())
	  ((= count num)
	   (iter (cdr xlst) 1))
	  (else
	   (cons (car xlst)
		 (iter (cdr xlst)
		       (+ count 1)))))))
(drop '(a b c d e f g h i k) 3)

(define (split lst at)
  ;; 17) Split a list into two parts; the length of the first part is given.
  (let iter ((from lst)
	     (to '())
	     (count at))
    (if (or (null? from)
	    (zero? count))
	(list (reverse to) from)
	(iter (cdr from)
	      (cons (car from) to)
	      (- count 1)))))
(split '(a b c d e f g h i k) 3)

(define (slice lst start stop)
  ;; 18) Extract a slice from a list
  (car (split (cadr (split lst (- start 1)))
	      (- stop (- start 1)))))
(slice '(a b c d e f g h i k) 3 7)

(define (rotate lst at)
  ;; 19) Roatate a list N places to the left.
  (if (< at 0)
      (reverse (rotate (reverse lst) (- at)))
      (append (cadr (split lst at))
	      (car (split lst at)))))
(rotate '(a b c d e f g h) 3)
(rotate '(a b c d e f g h) -2)

(define (remove-at lst at)
  ;; 20) Remove the K'th element from a list.
  (if (= at 1)
      (cdr lst)
      (cons (car lst)
	    (remove-at (cdr lst)
		       (- at 1)))))
(remove-at '(a b c d) 2)

(define (insert-at elem lst at)
  ;; 21) Insert an element at a give position into a list.
  (if (= at 1)
      (cons elem lst)
      (cons (car lst)
	    (insert-at elem
		       (cdr lst)
		       (- at 1)))))
(insert-at 'alfa '(a b c d) 2)

(define (range start stop)
  ;; 22) Create a list containing all integers withing a given range.
  (if (= start (+ stop 1))
      '()
      (cons start
	    (range (+ start 1)
		   stop))))
(range 4 9)

(define (rnd-select lst num)
  ;; 23) Extract a given number of randomly selected elements from a list
  (if (zero? num)
      '()
      (let ((at (random (length lst))))
	(cons (list-ref lst at)
	      (rnd-select (remove-at lst
				     (+ at 1))
			  (- num 1))))))
(rnd-select '(a b c d e f g h) 3)

(define (lotto-select num from)
  ;; 24) Lotto: Draw N different random numbers from the set 1..M
  (rnd-select (range 1 from) num))
(lotto-select 6 49)

(define (rnd-permu lst)
  ;; 25) Generate a random permutation of the elements of a list.
  (rnd-select lst (length lst)))
(rnd-permu '(a b c d e f))

(define (combination num lst)
  ;; 26) Generate the combinations of K distinct objects chosen from the N elements of a list
  (cond ((null? lst) '())
	((= num 1) (map list lst))
	(else
	 (append
	  (map
	   (lambda (subcomb)
	     (cons (car lst) subcomb))
	   (combination (- num 1) (cdr lst)))
	  (combination num (cdr lst))))))
(combination 3 '(a b c d e f))

(define (group lst form)
  ;; 27) Group the elements of a set into disjoint subsets
  (define (iter res lst form)
    (cond ((null? form) res)
	  ((null? res) '())
	  (else
	   (let* ((first-res (car res))
		  (rest
		   (filter
		    (lambda (elem)
		      (not
		       (member elem first-res)))
		    lst)))
	     (append
	      (map (lambda (rest-res)
		     (list first-res
			   rest-res))
		   (iter (combination (car form)
				      rest)
			 rest
			 (cdr form)))
	      (iter (cdr res) lst form))))))
  (iter (combination (car form) lst)
	lst
	(cdr form)))

(group '(aldbo beat carla david
	       evi flip gary
	       hugo ida)
       '(2 2 5))

(define (group3 lst)
  ;;  a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
  (group lst '(2 3 4)))
(group3 '(aldo beat carla david evi flip gary hugo ida))

(define (sort lst p)
  ;; 28) Sorting a list of lists according to length of sublists
  (cond ((null? lst) '())
	((null? (cdr lst)) lst)
	(else
	 (let* ((left
		 (filter (lambda (x)
			   (p x (car lst)))
			 (cdr lst)))
		(right
		 (filter (lambda (x)
			   (not (p x (car lst))))
			 (cdr lst))))
	   (append (sort left p)
		   (list (car lst))
		   (sort right p))))))

(define (lsort lst)
  ;; a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
  (sort lst (lambda (x y)
	      (< (length x) (length y)))))
(lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

(define (lfsort lst)
  ;; b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
  )

(lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

;; Arithmetic

(define (is-prime num)
   ;; 31) Determine whether a given integer number is prime
  (if (<= num 1)
      #f
      (let iter ((test (- num 1)))
	(if (eq? test 1)
	    #t
	    (and (not (eq? (modulo num test)
			   0))
		 (iter (- test 1)))))))
(is-prime 7)

(define (gcd x y)
  ;; 32) Determine the greatest common divisor of two positive integers numbers.
  (if (= y 0) x
      (gcd y (modulo x y))))
(gcd 36 63)

(define (coprime x y)
  ;; 33) Determine wheter two positive integers numbers are coprime
  (= (gcd x y) 1))
(coprime 35 64)

(define (totient-phi x)
  ;; 34) Calculate Euler's totient function phi(m)
  (define (iter x y count)
    (cond ((= x 1) 1)
	  ((= x y) count)
	  ((= (gcd x y) 1)
	   (iter x (+ y 1) (+ count 1)))
	  (else
	   (iter x (+ y 1) count))))
  (iter x 1 0))
(totient-phi 10)

(define (prime-factors prod)
  ;; 35) Determine the prime factors of a given positive integer.
  (define (iter prod count)
    (cond ((= prod 1) '())
	  ((and (= (modulo prod count) 0)
		(is-prime count))
	   (cons count
		 (iter (quotient prod count)
		       2)))
	  (else
	   (iter prod
		 (+ count 1)))))
  (iter prod 2))
(prime-factors 315)

(define (prime-factors-mult prod)
  ;; 36) Determine the prime factors of a given positive integers (2).
  (let ((fs (prime-factors prod)))
    (let iter ((first (car fs))
	       (rest (cdr fs))
	       (count 1))
      (cond ((null? rest)
	     (list (list first count)))
	    ((eq? first (car rest))
	     (iter first (cdr rest) (+ count 1)))
	    (else
	     (cons (list first count)
		   (iter (car rest)
			 (cdr rest) 1)))))))
(prime-factors-mult 315)

(define (totient-phi-improved x)
  ;; 37) Calculate Euler's totient function phi(m) (improved).
  (let iter ((fs (prime-factors-mult x))
	     (res 1))
    (if (null? fs)
	res
	(iter
	 (cdr fs)
	 (* res (* (- (caar fs) 1)
		   (expt (caar fs)
			 (- (cadar fs) 1))))))))
(totient-phi 10)
(totient-phi-improved 10)

;; 38) Compare the two methods of calculating Euler's totient function.
(totient-phi 10090)
(totient-phi-improved 10090)

(define (range-primes from to)
  ;; 39) A list of primes numbers
  (filter is-prime (range from to)))
(range-primes 1 100)

(define (goldbach num)
  ;; 40) Goldbach's conjecture.
  (let iter ((from (range-primes 1 num)))
    (cond
     ((null? from) num)
     ((is-prime (- num (car from)))
      (list (car from) (- num (car from))))
     (else
      (iter (cdr from))))))
(goldbach 28)

(define (goldbach-list from to greater)
  ;; 41) A list of Goldbach compositions.
  (filter
   (lambda (x) (> (car x) greater))
   (filter pair?
	   (map goldbach (range from to)))))
(goldbach-list 1 2000 50)

;; Logic and Codes

;; 46) 47) 48) Truth tables for logical expressions.

(define (gray x)
  ;; 49) Gray code.
  (cond ((= x 0) '())
	((= x 1) '("0" "1"))
	(else
	 (let ([next (gray (- x 1))])
	   (append
	    (map (lambda (s)
		   (string-append "0" s))
		 next)
	    (map (lambda (s)
		   (string-append "1" s))
		 next))))))
(gray 3)

(define (huffman x)
  ;; 50) Huffman code.
)
(huffman '((a 45) (b 13)
	   (c 12) (d 16)
	   (e 9) (f 5)))

(define (istree object)
  ;; 54A) Check wheater a given expression represents a binary tree
  (cond
   ((null? object) #t)
   ((not (= (length object) 3)) #f)
   (else
    (and (istree (cadr object))
	 (istree (caddr object))))))
(istree '(a (b () ()) ()))

