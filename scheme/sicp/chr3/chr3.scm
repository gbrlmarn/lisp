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

;; Representing tables
(define false #f)
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records))
	 (car records))
	(else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value)
			(cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

;; Two-dimensional tables
(define (lookup key-1 key-2 table)
  (let ((subtable
	 (assoc key-1 (cdr table))))
    (if subtable
	(let ((record
	       (assoc key-2 (cdr subtable))))
	  (if records
	      (cdr record)
	      false))
	false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc
		   key-1 (cdr table))))
    (if  subtable
	 (let ((record
		(assoc key-2
		       (cdr subtable))))
	   (if record
	       (set-cdr! record value)
	       (set-cdr!
		subtable
		(cons (cons key-2 value)
		      (cdr subtable)))))
	 (set-cdr!
	  table
	  (cons (list key-1
		      (cons key-2 value))
		(cdr table)))))
  'ok)

;; table with local state
(define false #f)
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1
		    (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2
			  (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1
		    (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2
			  (cdr local-table))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr!
		   subtable
		   (cons
		    (cons key-2 value)
		    (cdr subtable)))))
	    (set-cdr!
	     local-table
	     (cons
	      (list key-1 (cons key-2 value))
	      (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else
	     (error "Unknown operation.."))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 3.24
;; The only change that need is to
;; change the assoc procedure
;; to test equality using the
;; same-key? procedure isted of equal?

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records))
	     car records)
	    (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) #f)))))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr!
	     local-table
	     (cons (list key-1 (cons key-2 value))
		   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Invalid operation"))))
    dispatch))

;; 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond
       ((null? records) #f)
       ((equal? key (caar records))
	(car records))
       (else
	(assoc key (cdr records)))))
    (define (lookup keys)
      (define (iter rem-keys records)
	(cond
	 ((null? rem-keys) records)
	 ((not (pair? records)) #f)
	 (else
	  (let ((record
		 (assoc (car rem-keys)
			records)))
	    (if record
		(iter (cdr rem-keys)
		      (cdr record)))))))
      (iter keys (cdr local-table)))
    (define (insert! keys value)
      (define (iter rem-keys records)
	(cond
	 ((null? rem-keys)
	  (set-cdr! records value))
	 ((or (null? (cdr records))
	      (not (pair? (cdr records))))
	  (set-cdr!
	   records
	   (list
	    (cons (car rem-keys) '())))
	  (iter (cdr rem-keys)
		(car records)))
	 (else
	  (let ((record
		 (assoc (car rem-keys)
			(cdr records))))
	    (if record
		(iter (cdr keys) record)
		(begin
		  (set-cdr!
		   records
		   (cons (list (car rem-keys))
			 (cdr records)))
		  (iter (cdr rem-keys)
			(cadr records))))))))
      (iter keys local-table))
    (define (dispatch m)
      (cond
       ((eq? m 'lookup-proc) lookup)
       ((eq? m 'insert-proc!) insert!)
       (else
	(error "Invalid procedure"))))
    dispatch))

(define (lookup keys table)
  ((table 'lookup-proc) keys))
(define (insert! keys value table)
  ((table 'insert-proc!) keys value))

;; 3.26
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set record set)
  (cond
   ((null? set) (make-tree record '() '()))
   ((= (car record) (car (entry set))) set)
   ((< (car record) (car (entry set)))
    (make-tree (entry set)
	       (adjoin-set record (left set))
	       (right set)))
   ((> (car record) (car (entry set)))
    (make-tree
     (entry set)
     (left set)
     (adjoin-set record (right set))))))

(define (make-table)
  (let ((local-table '()))
    (define (lookup key records)
      (cond
       ((null? records) #f)
       ((= key (car (entry records)))
	(entry records))
       ((< key (car (entry records)))
	(lookup key (left records)))
       ((> key (car (entry records)))
	(lookup key (right records)))))
    (define (insert! key value)
      (let ((record (lookup key local-table)))
	(if record
	    (set-cdr! record value)
	    (set! local-table
		  (adjoin-set (cons key value)
			      local-table)))))
    (define (get key) (lookup key local-table))
    (define (dispatch m)
      (cond
       ((eq? m 'get) get)
       ((eq? m 'insert!) insert!)
       ((eq? m 'print) local-table)
       (else (error "Invalid operation"))))
    dispatch))

(define table (make-table))
(define get (table 'get))
(define put (table 'insert!))

(put 20 'a)
(put 21 'b)
(put 30 'c)
(put 79 'd)
(put 50 'e)
(table 'print)
(get 50)

;; 3.27

(define (memo-fib)
  (memorize
   (lambda (n)
     (cond ((= n 0) 0)
	   ((= n 1) 1)
	   (else (+ (memo-fib (- n 1))
		    (memo-fib (- n 2))))))))

;; the nth number will be computed in a
;; linear number of steps because
;; basically we iterate until n is 0

;; (define memo-bif (memorize fib))
;; Will not work because fib calls itself
;; before memorizing...


;; 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1)
		       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output
			       new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; 3.38
;; a. 45, 40, 35, 50
;; b. ...

;; 3.39
(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))
;; 101 121 11

;; 3.40
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
		  (lambda () (set! x (* x x x))))
;; 1 000 000 = (p1 10 * 10) -> (p2 100 * 100)
;; 10 000 = p1 = 10 * (p2 x = 1000)
;; 10 * 100 * 100 = 100 000
;; With serialized procedures
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
		  (s (lambda () (set! x (* x x x)))))
;; 1 000 000

;; 3.41
;; Balance check doesn't need to be serialized
;; because it doesn't change the state of the account

;; 3.42
;; Is the same just that the procedures operate under
;; the same serializer


;; Serializer aka Mutex implementation
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serialized-p)))

(define false #f)
(define true #t)
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)))
	    ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) true
      (begin (set-car! cell true)
	     false)))

;; 3.47
;; a) 
(define (make-semaphore n)
  (let ((lock (make-mutex))
	(counter 0))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
	     (lock 'acquire)
	     (if (< counter m)
		 (begin (set! counter (+ counter 1))
			(lock 'release))
		 (begin (lock 'release)
			(semaphore 'acquire))))
	    ((eq? m 'release)
	     (lock 'acquire)
	     (set! counter (- counter 1))
	     (lock 'release))
	    (else (error "Unknown command"))))
    semaphore))

;; Streams(delayed lists)

;; Classic sum of primes
(define (sum-primes a b)
  (define (iter count res)
    (cond ((> count b) res)
	  ((prime? count)
	   (iter (+ count 1) (+ res count)))
	  (else
	   (iter (+ count 1) res))))
  (iter a 0))

(define (prime? x)
  (= x (smallest-divisor x)))
(define (smallest-divisor x)
  (find-divisor x 2))
(define (find-divisor x count)
  (cond ((< x (square count)) x)
	((divides? count x) count)
	(else (find-divisor x
			    (+ count 1)))))
(define (square x)
  (* x x))
(define (divides? a b)
  (= (remainder b a) 0))

(sum-primes 1 10)

;; Sum of prime numbers
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
	    (enumerate-interval (+ low 1)
				high))))
(enumerate-interval 1 10)

(define (sum-primes a b)
  (accumulate + 0
	      (filter
	       prime?
	       (enumerate-interval a b))))
(define (accumulate proc start lst)
  (if (null? lst)
      start
      (accumulate proc
		  (proc start
			(car lst))
		  (cdr lst))))
(define (filter prop lst)
  (cond ((null? lst) '())
	((prop (car lst))
	 (cons (car lst)
	       (filter prop (cdr lst))))
	(else
	 (filter prop (cdr lst)))))

(filter prime? (enumerate-interval 1 10))
(accumulate + 0 (enumerate-interval 1 10))

;; Enumerate
;; Accumulate
;; Filter

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc
			      (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; Stream in action
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define the-empty-stream '())
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
				  high))))
(stream-enumerate-interval 100 1000)
(define (stream-null? stream)
  (null? stream))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream
	  (stream-car stream)
	  (stream-filter pred
			 (stream-cdr stream))))
	(else
	 (stream-filter pred
			(stream-cdr stream)))))

(cons 100
      (delay (stream-enumerate-interval 101
					500)))

(car
 (cdr
  (filter prime?
	  (enumerate-interval 100 200))))
(stream-car
 (stream-cdr
  (stream-filter prime?
		 (stream-enumerate-interval
		  10000 100000))))


(define false #f)
(define true #t)
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

(delay <exp>)
;; Delay is equivalent to
(memo-proc (lambda () <exp>))

(eqv? (delay <exp>)
	(memo-proc (lambda () <exp>)))


;; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc
		    (map stream-cdr argstreams))))))

;; 3.51
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
	      (stream-enumerate-interval 0 10)))
;; 0 1 2 3 4 5 6 7 8 9 10
(stream-ref x 5)
;; 5
(stream-ref x 7)
;; 7

;; 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum)) sum)
(define seq (stream-map
	     accum
	     (stream-enumerate-interval 1 20)))
;; sum = 210
(display-stream seq)
(define y (stream-filter even? seq))
(display-stream y)
(define z
  (stream-filter
   (lambda (x) (= (remainder x 5) 0))
   seq))
(display-stream z)

(stream-ref y 7)
(display-stream z)

;; 3.52 Infinite Streams
;; Macro for delay
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       (expr)))))
;; Macro for cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (integers-starting-from n)
  (cons-stream
   n
   (lambda () (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))
(define (divisibile? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter
   (lambda (x) (not (divisibile? x 7)))
   integers))

(stream-ref no-sevens 100)
