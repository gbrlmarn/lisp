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
	(cond
	 ((null? from) (list to))
	 ((equal? (car to)
		  (car from))
	  (iter (cdr from)
		(cons (car from)
		      to)))
	 (else
	  (cons to
		(iter (cdr from)
		      (list (car from)))))))))
(pack '(a a a a b c c a a d e e e e))

(define (encode lst)
  ;; 10) Run-length encoding of a list
  (let iter ((packed (pack lst)))
    (if (null? packed)
	'()
	(cons (list (length (car packed))
		    (car (car packed)))
	 (iter (cdr packed))))))
(encode '(a a a a b c c a a d e e e e))

(define (encode-modified lst)
  ;; 11) Modified run-length encoding
  (let iter ((packed (pack lst)))
    (cond ((null? packed) '())
	  ((= (length (car packed)) 1)
	   (cons (car (car packed))
		 (iter (cdr packed))))
	  (else
	   (cons (list (length (car packed))
		       (car (car packed)))
		 (iter (cdr packed)))))))
(encode-modified '(a a a a b c c a a d e e e e))

(define (decode lst)
  ;; 12) Decode a run-length encoded list.
  (define (constructor times symbol)
    (if (= times 0)
	'()
	(cons symbol (constructor (- times 1) symbol))))
  (define (decode-one encoding)
    (if (pair? encoding)
	(constructor (car encoding) (cadr encoding))
	(list encoding)))
  (if (null? lst)
      '()
      (append (decode-one (car lst))
	      (decode (cdr lst)))))

(decode '((4 A) B (2 C) (2 A) D (4 E)))

(define (encode-direct lst)
  ;; 13) Run-length encoding of a list(direct solution)
  (if (null? lst)
      '()
      (let ((format (lambda (counter elem)
		      (if (= counter 1)
			  elem
			  (list counter elem)))))
	(let iter ((elem (car lst))
		   (rest (cdr lst))
		   (counter 1))
	  (cond
	   ((null? rest)
	    (list (format counter elem)))
	   ((eq? elem (car rest))
	    (iter elem (cdr rest) (+ counter 1)))
	   (else
	    (cons (format counter elem)
		  (iter (car rest)
			(cdr rest)
			1))))))))
(encode-direct '(a a a a b c c a a d e e e e))

(define (dupli lst)
  ;; 14) Duplicate the elements of a list
  (if (null? lst)
      '()
      (append (list (car lst)
		    (car lst))
	      (dupli (cdr lst)))))
(dupli '(a b c c d))

(define (repli lst num)
  ;; 15) Replicate the elements of a list a given number of times
  (if (null? lst)
      '()
      (let iter ((xlst lst)
		 (res '())
		 (count 0))
	(cond
	 ((null? xlst) res)
	 ((= count num)
	  (iter (cdr xlst)
		res
		0))
	 (else
	  (iter xlst
		(append res (list (car xlst)))
		(+ count 1)))))))
(repli '(a b c) 3)

(define (drop lst num)
  ;; 16) Drop every N'th element from a list.
  (let iter ((xlst lst)
	     (count 1))
    (cond ((null? xlst) '())
	  ((= count num)
	   (iter (cdr xlst) 1))
	  (else
	   (cons (car xlst)
		 (iter (cdr xlst)
		       (+ count 1)))))))
(drop '(a b c d e f g h i k) 3)
