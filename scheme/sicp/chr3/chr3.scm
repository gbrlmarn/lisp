(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
(withdraw 20)

;; withdraw with internal balance
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance
		       (- balance amount))
		 amount)
	  "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))
(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))
(w1 10)
(w1 0)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else
	   (error "Unknown request: MAKE-ACCOUNT"
		  m))))
  dispatch)

(define acc (make-account 100))
((acc 'deposit) 10)
((acc 'withdraw) 10)

;; 3.1
(define (make-accumulator start)
  (lambda (increment)
    (set! start (+ start increment))
    start))
(define A (make-accumulator 5))
(A 10)

;; 3.2
(define (make-monitored f)
  (define times-called 0)
  (define (mf msg)
    (cond ((eq? 'how-many-calls? msg) times-called)
	  ((eq? 'reset-count msg) (set! times-called 0))
	  (else
	   (set! times-called (+ times-called 1))
	   (f msg))))
  mf)

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)

;; 3.2 again with let
(define (make-monitored f)
  (let ((times-called 0))
    (define (mf msg)
      (cond ((eq? 'how-many-calls? msg) times-called)
	    ((eq? 'reset-count msg)
	     (set! times-called 0))
	    (else
	     (set! times-called (+ times-called 1))
	     (f msg))))
    mf))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

;; 3.3 
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin	(set! balance (- balance amount))
		balance)
	 "Insufficient funds..."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass msg)
    (if (eq? pass password)
	(cond ((eq? msg 'withdraw) withdraw)
	      ((eq? msg 'deposit) deposit)
	      (else
	       (error "Option not present...")))
	(error "Incorrect password...")))
  dispatch)
;; testing
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'deposit) 50)

;; 3.4
(define (make-account balance password)
  (let ((count 0)
	(limit 7))
    (define call-the-cops "Calling 991")
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin
	    (set! balance (- balance amount))
	    balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (define (dispatch pass m)
      (if (eq? pass password)
	  (begin
	    (set! count 0)
	    (cond
	     ((eq? m 'withdraw) withdraw)
	     ((eq? m 'deposit) deposit)
	     (else
	      (error "Wrong operation..."))))
	  (begin
	    (if (>= count limit)
		(call-the-cops)
		(set! count (+ count 1)))
	    (error "Wrong password"))))
  dispatch))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'nosecret 'withdraw) 40)


;; 3.6 
(define rand
  (let ((x rand-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define rand-init 0)
(define (rand-update x)
  (+ x 1))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(random-in-range 1 10)
(rand)

(define rand
  (let ((x rand-init))
    (define (dispatch msg)
      (cond ((eq? msg 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? msg 'reset)
	     (lambda (new-value)
	       (set! x new-value)))))
    dispatch))
(define rand-init 0)
(define (rand-update x) (+ x 1))
;; testing
(rand 'generate)
(rand 'generate)
((rand 'reset) 0)
((rand 'reset) 10)
(rand 'generate)

;; How state complicates things...

;; has state
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define my-bank (make-simplified-withdraw 20))
(my-bank 5)

;; doesn't have state

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define my-stateless-bank (make-decrementer 20))
(my-stateless-bank 10)

;; Factorial iterative
;; time  O(n) = n
;; space O(n) = 1
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))
(factorial 3)

;; Factorial recursive
;; time  O(n) = n
;; space O(n) = n
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 3)

;; Imperative factorial
(define (factorial n)
  (let ((product 1)
	(counter 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin (set! product (* counter product))
		 (set! counter (+ counter 1))
		 (iter))))
    (iter)))
;; The leason is...because of the imperative nature,
;; if we change places of the instructions:
;; (set! product ...) and (set! counter ...)
;; the program will be incorrect...
;; Programming with assignment forces us to
;; carefully consider the order of those assignments

;; 3.7


;; 3.3 
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin	(set! balance (- balance amount))
		balance)
	 "Insufficient funds..."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass msg)
    (if (eq? pass password)
	(cond ((eq? msg 'withdraw) withdraw)
	      ((eq? msg 'deposit) deposit)
	      (else
	       (error "Option not present...")))
	(error "Incorrect password...")))
  dispatch)

(define (make-joint account password join-password)
  (define (withdraw amount)
    ((account password 'withdraw) amount))
  (define (deposit amount)
    ((account password 'deposit) amount))
  (define (dispatch pass msg)
    (if (eq? pass join-password)
	(cond ((eq? msg 'withdraw) withdraw)
	      ((eq? msg 'deposit) deposit)
	      (else
	       (error "Option not present...")))
	(error "Incorrect joint password...")))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rose-bud))
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rose-bud 'deposit) 10)

;; 3.8
(define f
  (let ((start 0))
    (lambda (x)
      (set! start (- x start))
      (- x start))))
(f 0)
(f 1)
(+ (f 0) (f 1))
(+ (f 1) (f 0))


;; Procedures and environments
(define (square x)
  (* x x))
(square 3)
;; Equivalent :D
(define square
  (lambda (x)
    (* x x)))
(square 3)

;; Applying Simple Procedures
(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;; 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 3)
;; E1 n:3 (* 3 (factorial 2))
;; E2 n:2 (* 2 (factorial 1))
;; E3 n:1 1 
(define (factorial n)
  (define (fact-iter product counter max)
    (if (> counter max)
	product
	(fact-iter (* product counter)
		   (+ counter 1)
		   max)))
  (fact-iter 1 1 n))
(factorial 3)
;; E1 n:3 (factorial 3)
;; E2 product:1
;;    counter:1
;;    max:3
;;    (fact-iter 1 1 3)
;; E3 product: 1
;;    counter: 2
;;    max:     3
;;    (fact-iter 1 2 3)
;; E4 product: 2
;;    counter: 3
;;    max:     3
;;    (fact-iter 2 3 3)
;; E5 product: 6
;;    counter: 4
;;    max:     3
;;    6

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance
		     (- balance amount))
	       balance)
	(error "Insufficinet funds"))))
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))

;; 3.10
;; (let ((<var> <exp>)) <body>)
;; ((lambda (<var>) <body>) <exp>)




