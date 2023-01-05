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

(define (make-withdraw balance)
  (let ((initial-balance balance))
    (lambda (amount)
      (if (>= initial-balance amount)
	  (begin (set! initial-balance
		       (- initial-balance
			  amount))
		 initial-balance)
	  (error ("Insufficient funds"))))))
(define W1 (make-withdraw 100))
(W1 50)

;; lambda version
;; EG: make-withdraw
;;     W1: make-withdraw 100
;;     balance: 100
;; E1->EG: balance: 50
;;         amount: 50
;; E2->EG: balance: 0
;;         amount: 50
;; E3->EG: insufficient funds...

;; let version
;; EG: make-withdraw
;;     W1: make-withdraw 100
;;     balance: 100
;; E1->EG: initial-balance: 100
;; E2->E1: amount: 50
;; E3->EG: initial-balance: 50
;; E4->E3: amount: 0
;; E5->EG: initial-balance 0
;; E6->E5: 'insufficient funds'

;; Internal definitions

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(define (average x y)
  (/ (+ x y) 2))
(sqrt 10)
;; GE: (sqrt x)
;; E1->GE: x:2
;;         good-enough? improve sqrt-iter
;; E2->E1: guess: 1 sqrt-iter
;; E3->E1: guess: 1 good-enough?

;; 3.11
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance
		     (- balance amount))
	       balance)
	(error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else
	   (error "Unknown procedure: " m))))
  dispatch)

(define acc (make-account 50))
;; EG: acc make-account
;; E1->EG: balance: 50 withdraw deposit dispatch
((acc 'deposit) 40)
;; E2->E1: m: 'deposit
;; E3->E2: amount: 40
((acc 'withdraw) 60)
;; E2->E1: m: 'withdraw
;; E3->E2: amount: 60

(define acc2 (make-account 100))
;; Where is the local state for acc kept?
;; The local state is kept in balance
;; that is created when the make-account
;; is called.

;; How are the local states for the two
;; accounts keps distinct?
;; They are distinct because every call
;; of make-account creates a new
;; environment where balance has a
;; certain number

;; Which parts of the env structure
;; are shared between acc and acc2?
;; Everyting is shared between them
;; except that they are created in
;; different environments with different
;; local balance variable

;; Mutable list structure
(define x '((a b) c d))
(define y '(e f))

(display x)
(car x y)
(define z (cons y (cdr x)))

;; Define new cons
(define (new-cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;; 3.12
(define (new-append x y)
  (if (null? x)
      y
      (cons (car x)
	    (new-append (cdr x) y))))
(define (new-append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;;(a b c d)
(cdr x)
;; (b)
(define w (append! x y))
w
;; (a b c d)
(cdr x)
;; (b c d)

;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
;;(last-pair z)
;; z will be a -> b-> c
;;           |        |
;;           \--------/
;; the command last-pair will create an
;; infinite recursion because the list
;; 'z' is circular and never ends and
;; it's dangerous :D

;; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
(display v)
;; |a|*| -> |b|*| -> |c|*| -> |d| |
(define w (mystery v))
(display w)
;; E1->EG: temp: (b c d) x: (a)
;; E2->E1: temp: (c d) x: (b a)
;; E3->E2: temp: (d) x: (c b a)
;; E4->E3: temp: () x: (d c b a)
;; |d|*| -> |c|*| -> |b|*| -> |a| |

;; Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(display z1)
(define z2 (cons
	    (list 'a 'b)
	    (list 'a 'b)))
(display z2)
(define (set-wow! x)
  (set-car! (car x) 'wow) x)
(eq? z1 z2)

;; 3.15
;; z1
;; |*|*|
;;  | |
;;  | \---> ('wow b)
;;  \---> ('wow b)
;; z2
;; |*|*|
;;  | |
;;  | \---> ('wow b)
;;  \---> (a b)

;; 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
(define str1 '(a b c))
(count-pairs str1)
;; returns 3
;;( . ) -> ( . ) -> ( . ) -> null
;; |        |        |
;; V        V        V
;;'a       'b       'c
(define x '(a))
(define y (cons x x))
(define str2 (list y))
(count-pairs str2)
;; returns 4
;; ( . ) -> null
;;  |
;;  V
;; ( . )
;;  | |
;;  V V
;; ( . ) -> null
;;  |
;;  V
;; 'a

(define str3 (cons y y))
(count-pairs str3)
;; returns 7
;; ( . )
;;  | |
;;  V V
;; ( . )
;;  | |
;;  V V
;; ( . ) -> null
;;  |
;;  V
;; 'a

;; 3.17
(define (count-pairs x)
  (let ((found '()))
    (define (helper x)
      (if
       (or (not (pair? x))
	   (memq x found))
       0
       (begin
	 (set! found (cons x found))
	 (+ (helper (car x))
	    (helper (cdr x))
	    1))))
    (helper x)))

;; Testing
(count-pairs str1)
(count-pairs str2)
(count-pairs str3)

;; 3.18
(define (cycle? lst)
  (let ((visited '()))
    (define (helper x)
      (set! visited (cons x visited))
      (cond ((null? (cdr x)) #f)
	    ((memq (cdr x) visited) #t)
	    (else (helper (cdr x)))))
    (helper lst)))
(cycle? (list 'a 'b 'c))
(cycle? z)

;; 3.19
;; ...

;; 3.20
(define x (cons 1 2))
;; (1 . 2)
(define z (cons x x))
;; (  .  )
;;  |   |
;;  V   V
;;  x   x
;; ((1 . 2) 1 . 2)
(set-car! (cdr z) 17)
;; (cdr z) => x
;; (set-car! x 17) => (17 . 2)
;; (z) => ((17 . 2) 17 . 2)
;; ((17 . 2) 17 . 2)


;; Representing Queues
;; Queue = FIFO
;; One constructor -> (make-queue)
;; Two selectors ->
;;  (empty-queue? <queue>)
;;  (front-queue <queue>)
;; Two mutators ->
;;  (insert-queue! <queue> <item>)
;;  (delete-queue! <queue>)

;; Queue representation
;; q-> ( . )---rear-ptr--------------\
;;      |                            V 
;;      \-front-ptr-> (a. )->(b. )->(c. )
;;                                     |
;;                                     V
;;                                    null

;; Queue implementation
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Queue is empty")
      (car (front-ptr queue))))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Queue is empty")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue)
		     new-pair)
	   (set-rear-ptr! queue
			  new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "Queue allready empty"))
	(else
	 (set-front-ptr!
	  queue (cdr (front-ptr queue)))
	 queue)))

;; 3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

(define (print-queue queue)
  (car queue))

;; Testing 
(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

;; 3.22
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (empty-q?)
      (null? front-ptr))
    (define (insert-q! item)
      (let ((new-pair (cons item '())))
	(cond
	 ((empty-q?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair)
	  front-ptr)
	 (else
	  (set-cdr! rear-ptr new-pair)
	  (set! rear-ptr new-pair)
	  front-ptr))))
    (define (delete-q!)
      (cond
       ((empty-q?)
	(error "Queue already empty"))
       (else
	(set! front-ptr (cdr front-ptr))
	front-ptr)))
    (define (dispatch m)
      (cond
       ((eq? m 'insert-q!) insert-q!)
       ((eq? m 'delete-q!) (delete-q!))
       ((eq? m 'empty-q?) (empty-q?))
       (else
	(error "Incorrect opperation."))))
    dispatch))

;; Testing
(define q1 (make-queue))
((q1 'insert-q!) 'b)
((q1 'insert-q!) 'a)
(q1 'delete-q!)
(q1 'empty-q?)

;; 3.23
;; Deque("double-ended queue")
(define (make-deque)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty-deque?)
      (null? front-ptr))
    (define (front-insert-deque! item)
      (let ((new-pair
	     (list '() item '())))
	(cond
	 ((empty-deque?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair))
	 (else
	  (set-cdr! (cdr new-pair) front-ptr)
	  (set-car! front-ptr new-pair)
	  (set! front-ptr new-pair)))
	front-ptr))
    (define (rear-insert-deque! item)
      (let ((new-pair
	     (list '() item '())))
	(cond
	 ((empty-deque?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair))
	 (else
	  (set-car! new-pair rear-ptr)
	  (set-cdr!
	   (cdr rear-ptr) new-pair)
	  (set! rear-ptr new-pair)))
	front-ptr))
    (define (front-delete-deque!)
      (set! front-ptr (cddr front-ptr))
      (set-car! front-ptr '())
      front-ptr)
    (define (rear-delete-deque!)
      (set! rear-ptr (car rear-ptr))
      (set-car! (cddr rear-ptr) '())
      front-ptr)
    (define (print-deque)
      (define (iter from to)
	(if (null? (caddr from))
	 (append to (cons (cadr from) '()))
	 (iter
	  (cddr from)
	  (append to (cons (cadr from) '())))))
      (iter front-ptr '()))
    (define (dispatch m)
      (cond
       ((eq? m 'empty-deque?)
	empty-deque?)
       ((eq? m 'print-deque)
	print-deque)
       ((eq? m 'front-insert-deque!)
	front-insert-deque!)
       ((eq? m 'rear-insert-deque!)
	rear-insert-deque!)
       ((eq? m 'front-delete-deque!)
	front-delete-deque!)
       ((eq? m 'rear-delete-deque!)
	rear-delete-deque!)
       (else
	(error "Invalid operation"))))
    dispatch))

;; testing
(define dq1 (make-deque))
((dq1 'rear-insert-deque!) 'a)
((dq1 'print-deque))
((dq1 'rear-insert-deque!) 'b)
((dq1 'rear-insert-deque!) 'c)
((dq1 'rear-insert-deque!) 'd)
((dq1 'print-deque))
((dq1 'front-insert-deque!) 'e)
((dq1 'front-insert-deque!) 'f)
((dq1 'front-insert-deque!) 'g)
((dq1 'front-insert-deque!) 'h)
((dq1 'print-deque))
((dq1 'front-delete-deque!))
((dq1 'front-delete-deque!))
((dq1 'print-deque))
((dq1 'rear-delete-deque!))
((dq1 'rear-delete-deque!))
((dq1 'print-deque))

