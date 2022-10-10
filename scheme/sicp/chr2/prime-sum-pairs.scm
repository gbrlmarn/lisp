(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(prime-sum-pairs 5)

(define nil '())

(define (enumerate-interval low high)
  (if (> low high) nil
    (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 1 5)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate proc flat seq)
  (if (null? seq) 
    flat
    (proc (car seq)
          (accumulate proc flat (cdr seq)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
        ((divide? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))))
(define (divide? a b)
  (= (remainder b a) 0))
  
