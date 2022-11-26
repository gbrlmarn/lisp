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



    
