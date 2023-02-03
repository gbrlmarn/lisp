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
	(cond ((null? from) (list to))
	      ((equal? (car to)
		       (car from))
	       (iter (cdr from)
		     (cons (car from)
			   to)))
	      (else
	       (cons
		to
		(iter (cdr from)
		      (list (car from)))))))))
(pack '(a a a a b c c a a d e e e e))


	    
