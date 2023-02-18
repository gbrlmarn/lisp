;; From url:
;; https://ds26gte.github.io/tyscheme/index-Z-H-10.html

;; Macros 
(define-macro when
  (lambda (test . branch)
    (list 'if test
	  (cons 'begin branch))))

(apply
 (lambda (test . branch)
   (list 'if test
	 (cons 'begin branch)))
 '((< 10 20)
   (display "Greater")))

(define-macro unless
  (lambda (test . branch)
    (list 'if
	  (list 'not test)
	  (cons 'begin branch))))

(list 'if test
      (cons 'begin branch))
;; Is equivalent to
`(if ,test
     (begin ,@branch))

(define-macro when
  (lambda (test . branch)
    `(if ,test
	 (begin ,@branch))))

(or #f 2)

(define-macro my-or
  (lambda (x y)
    (let ((temp (gensym)))
      `(let ((,temp ,x))
	 (if ,temp ,temp ,y)))))

(my-or #f 3)


;; Expression
(fluid-let ((x 9) (y (+ y 1)))
  (+ x y))

;; We want the expansion to be
(let ((OLD-X x) (OLD-Y y))
  (set! x 9)
  (set! y (+ y 1))
  (let ((RESULT (begin (+ x y))))
    (set! x OLD-X)
    (set! y OLD-Y)
    RESULT))

(map (gensym) '((first 1) (second 2)))
(map (lambda (ig) (cons (gensym) (cdr ig)))
     '((first 1) (second 2) (third 3)))

(cons (gensym) '3)

(my-append '(1 2 3) '(4 5 6))

(list 'x 'is x)
`(x is ,x)

(quasiquote (0 (unquote-splicing 1)))
`(a b c ,1 ,@())

(apply (lambda (y) (+ y)) '(x))

(define double
  (lambda (x) (+ x x)))
double
(double 2)
