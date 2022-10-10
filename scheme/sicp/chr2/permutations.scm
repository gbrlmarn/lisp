;; Plan:
;; ---
;; For each item x in S, recursively generate the sequence of
;; permutation of S - x, and adjoin x to the front of each one.

(define (permutations seq)
  (if (null? seq)
    (list nil)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x seq))))
             seq)))
(permutations (list 1 2 3))

(define nil '())

(define (flatmap proc seq)
  (accumulator append nil (map proc seq)))

(define (accumulator op initial seq)
  (if (null? seq) 
    initial
    (op (car seq)
        (accumulator op initial (cdr seq)))))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (filter proc seq)
  (cond ((null? seq) nil)
        ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))

