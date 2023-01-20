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

(define fold-left
  (lambda (f lst res)
    (if (null? lst)
	res
	(fold-left
	 f
	 (cdr lst)
	 (cons (car lst) res)))))
(define reverse
  (lambda (lst)
    (fold-left cons lst '())))
(reverse '(a b c d))

(define (reverse lst)
  (define (iter lst res)
    (if (null? lst)
	res
	(iter (cdr lst)
	      (cons (car lst) res))))
  (iter lst '()))
(reverse '(a b c d))

(define (palindrome lst)
  (equal? (reverse lst) lst))
(palindrome '(x a m a x))
