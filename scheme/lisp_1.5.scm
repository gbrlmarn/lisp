(define (my-equal? expr1 expr2)
  (cond ((not (pair? expr1))
	 (cond ((not (pair? expr2))
		(eq? expr1 expr2))
	       #t
	       (else #f))
	 ((my-equal? (car expr1)
		     (car expr2))
	  (my-equal? (cdr expr1)
		     (cdr expr2)))
	 #t
	 (else #f))))
