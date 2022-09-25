(define (fib x)
  (cond 
    ((= x 0) 0)
    ((= x 1) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))

(define (fib x)
  (define (fib-iter prev sum count)
    (if (= count 0) sum
        (fib-iter (+ sum prev) prev (- count 1))))
  (fib-iter 1 0 x))

(+ 2 2)

(define (add-interval x y)
  (make-interval (+ (lower-bond x) (lower-bond y))
                 (+ (upper-bond x) (upper-bond y))))

(cons (cons 1 2)
      (cons 3 4))
(cons (cons 1 (cons 2 3)) 4)

(define one-through-four (list 1 2 3 4))

(car one-through-four)
(cdr one-through-four)

(car (cdr (cdr one-through-four)))
(cons 10 one-through-four)

(define (list-ref items n)
  (if (= n 0) (car items)
    (list-ref (cdr items) (- n 1))))

;; recursive style
(define (length items)
  (if (null? items) 0
    (+ 1 (length (cdr items)))))

(length one-through-four)

;; iterative style
(define (length items)
  (define (length-iter a count)
    (if (null? a) count
    (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(length one-through-four)

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))
(append odds squares)

(cdr (list2))

(define (last-pair items)
  (if (= (length items) 2) (cdr items)
    (last-pair (cdr items))))

(last-pair squares)

(cons 1 (cons 2 '()))

(length (cons 1 '()))
(length (list 1 2))

(define (reverse items)
  (define (helper a rev)
    (if (= (length a) 1) (append a rev)
      (helper (cdr a) (cons (car a) rev))))
  (helper items '()))

(reverse squares)

(define nil '())

(define (reverse items)
  (define (helper a rev)
    (if (null? a) rev
      (helper (cdr a) (append (car a) rev))))
  (helper items nil))
(reverse squares)

;; ex 2.20
(define (same-parity x . y)
  (define (helper a rev)
    (cond ((null? a) rev)
          ((even? (+ x (car a)))
           (helper (cdr a) (append rev (list (car a)))))
          (else (helper (cdr a) rev))))
  (helper y (list x)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 1 3 4 5 6 7)


(car (cdr (cdr (list 1 2 3 4 5))))


(append nil (list (car '(1 2 3))))

(define (scale-list items factor)
  (if (null? items) nil
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(map abs (list -10 2.5 -11.47 17))

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)

(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4 5))

;; ex 2.21
;; square list 
;; cons version
(define (square-list items)
  (if (null? items) nil
    (cons (square (car items)) (square-list (cdr items)))))
(square-list (list 1 2 3 4 5))
;; map version
(define (square-list items)
  (map (lambda (x) (square x))
       items))
(square-list (list 1 2 3 4 5))

;; ex 2.22
;; iter square-list
(define test-list (list 1 2 3 4 5))
(define nil '())
;; wrong version
(define (square-list items)
  (define (iter things answer)
    (if (null? things) answer
      (iter (cdr things)
            (cons (square (car things)) answer))))
  (iter items nil))
;; working version
(define (square-list items)
  (define (iter things answer)
    (if (null? things) answer
      (iter (cdr things)
            (append  answer (list (square (car things)))))))
  (iter items nil))
(square-list test-list)
;; testing
(append (list 1) '())
(cons (cons '(1) '(2)) '(3))
(append (append '(1) '(2)) '(3))

;; ex 2.23
(display 3)
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
;; for-each implementation
(define (for-each-new expr items)
  (if (null? items) #t
    (and (expr (car items)) (for-each-new expr (cdr items)))))
(for-each-new (lambda (x) (newline) (display x))
              (list 1 2 3 4))
