;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metalinguistic Abstraction   ;;
;; SICP Chapter 4               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "the most fundamental idea in
;; programming:
;; The evaluator, which determines the
;; meaning of expresions in a programming
;; language, is just another program."

;; Definition of eval:
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp)
	 (lookup-varialle-value exp env))
	((assignment? exp)
	 (eval-assignment exp env))
	((definition? exp)
	 (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond-if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type: EVAL" exp))))

;; Apply
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type: APPLY" procedure))))

;; Conditionals
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (my-eval (first-exp exps) env))
	(else
	 (my-eval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (my-eval (assignment-value exp) env)
   env)
  'ok)
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; 4.1
;; Clasic version
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval
	     (first-operand exps) env)
	    (list-of-values
	     (rest-operands exps) env))))
;; left to right
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first
	      (list-of-values-lr
	       (rest-operands exps)
	       env)))))
;; right to left
(define (list-of-values-rl exps env)
  (list-of-values-lr (reverse exps)
		     env))
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
	      (list (car lst)))))
(reverse '(a b c d e f g))

(append '(a) '(b c))
(cons '(a) '(b c))


;; 4.1.2 Representing Expressions
(define false #f)
(define true #t)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Assignments have the form
;; (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

;; Definitions have the form
;; (define <var> <value>)
;; or
;; (define (<var> <param-1>...<param-n>))
;; equivalent to
;; (define <va>
;;   (lambda (<param-1> ... <param-n>)
;;     <body>))

(define (definition? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (caddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (caddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate
		 consequent
		 alternative)
  (list 'if
	predicate
	consequent
	alternative))
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; Derivated expressions

(define cond-expr
  '(cond ((> x 0) x)
	((= x 0) (display 'zero) 0)
	(else (- x))))

(define (tagged-list? exp with)
  (eq? (car exp) with))
(define (cond? exp)
  (tagged-list? exp 'cond))
(cond? cond-expr)

(define (cond-clauses exp)
  (cdr exp))
(cond-clauses cond-expr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(cond-else-clause?
 (cond-clauses (cond-expr)))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp
		 (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp
		      (cond-actions first))
		     (expand-clauses rest))))))

(expand-clauses (cond-clauses cond-expr))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (make-begin seq)
  (cons 'begin seq))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; cond expression evaluation
(define cond-exp
  '(cond ((> x 0) x)
	 ((= x 0) (display 'zero) 0)
	 (else (- x))))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (tagged-list? exp with)
  (eq? (car exp) with))
(cond? cond-exp)

(define (cond-clauses exp)
  (cdr exp))
(cond-clauses cond-exp)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond-if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(cond-if cond-exp)


;; 4.2

 ;; a) Assignment expressions are technically pairs and will be evaluated as applications. Evaluating an assignment as an application will cause the evaluator to try to evaluate the assignment variable instead of treating it as a symbol. 

;; a) Assignment expressions are pairs and
;;    will be evaluated as applications.
;;    Evaluating assignments as applications
;; b)
(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(define test-module (module-gensym))
(eval (+ 2 2) (interaction-environment))
(apply + '(1 2 3))

(define (new-eval exp env)
  ;; 4.3 Rewrite eval so that the dispatch is done in data-directed style...
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((assq (car exp) eval-rules)
	 => (lambda (type-rule-pair)
	      ((cdr type-rule-pair) exp env)))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp)
				env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define eval-rules
  (list
   (cons 'quote (lambda (exp env) (text-of-quotation exp)))
   (cons 'set! eval-assignment)
   (cons 'define eval-definition)
   (cons 'if eval-if)
   (cons 'lambda
	 (lambda (exp env)
	   (make-procedure
	    (lambda-parameters exp)
	    (lambda-body exp)
	    env)))
   (cons 'begin
	 (lambda (exp env)
	   (eval-sequence (begin-actions exp)
			  env)))
   (cons 'cond (lambda (exp env)
		 (eval (cond->if exp) env)))))


(define (eval-and exps env)
  ;; 4.4 eval-and and eval-or.
  (cond
   ((no-predicates? exps) true)
   ((not (true? (eval (first-predicate exps)
		      env))) false)
   (else (eval-and (rest-predicates exps) env))))

(define (no-predicates? exps) (null? exps))
(define (true? exp) (eq? exp #t))
(define (first-predicate exp) (car exp))
(define (rest-predicates exp) (cdr exp))

(define (eval-or exps env)
  (cond
   ((no-predicates? exps) false)
   ((true? (eval (first-predicate exps) env))
    true)
   (else
    (eval-or (rest-predicates exps) env))))

(define (expand-clauses clauses)
  ;; 4.5 ...Modify the handling of cont so that it supports (<test> => <recipient>)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp
		 (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (if (eq? (car (cond-actions first))
		     '=>)
		(make-if
		 (cond-predicate first)
		 (list
		  (cadr (cond-actions first))
		  (cond-predicate first))
		 (expand-claouses rest env))
		(make-if
		 (cond-predicate first)
		 (sequence->exp
		  (cond-actions first))
		 (expand-clauses rest env)))))))

(define (let->combination expr)
  ;; 4.6 Implement a syntactic transformation let->combination 
  (list (make-lambda (let-vars expr)
		     (let-body expr))
	(let-inits expr)))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))
(define (let-vars exp) (map car (cadr exp)))
(define (let-body exp) cddr exp)
(define (let-inits exp) (map cadr (cadr exp)))

(define (let*->nested-lets exp)
  ;; 4.7 let* is similar to let, except that the bindings are perfomed sequencialy from left to rightn
  (let ((inits (cadr exp))
	(body (cddr exp)))
    (define (make-lets exprs)
      (if (null? exprs)
	  body
	  (list 'let (list (car exprs))
		(make-lets (cdr exprs)))))
    (make-lets inits)))

(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))
;; <=>
(let ((x 3))
  (let ((y (+ x 2)))
    (let (z (+ x y 5))
      (* x z))))


(define (let->combination expr)
  ;; 4.8 Named let implementation (let <var> <bindings> <body>)
  (if (named-let? expr)
      (sequence->exp 
       (list (named-let->func expr) 
             (cons (named-let-func-name expr)
		   (named-let-func-inits expr))))
      (list (make-lambda (let-vars expr)
			 (let-body expr))
	    (let-inits expr))))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (first-exp seq) (car seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (named-let? expr)
  ((and (let? expr)
	(symbol? (cadr expr)))))
(define (let? expr)
  (eq? (car expr) 'let))
(define (named-let-func-name expr) (cadr expr))
(define (named-let-func-body expr) (cadddr expr))
(define (named-let-func-params expr)
  (map car (caddr expr)))
(define (named-let-func-inits expr)
  (map cadr (caddr expr)))
(define (named-let->func expr)
  (list 'define
	(cons (named-let-func-name expr)
	      (named-let-func-params expr))
	(named-let-func-body expr)))

;; 4.9 Implement while, do, until. While form is (while <predicate> <body>)
(define (while? expr)
  (tagged-list? (car expr) 'while))
(define (while-predicate expr) (cadr expr))
(define (while-body expr) (caddr expr))
(define (eval-while expr env)
  (let ((pred (while-predicate expr))
	(body (while-body expr)))
    (eval
     (make-if pred
	      (sequence->exp (list body expr))
	      "done")
     env)))
(define (make-if pred conseq alt)
  (list 'if pred conseq alt))
(define (tagged-list? lst tag)
  (if (pair? lst)
      (eq? (car lst) tag)
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I like this              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a 0)		    ;;
      (res '()))	    ;;
  (while (< a 10)	    ;;
    (set! res (cons a res)) ;;
    (set! a (+ a 1)))	    ;;
  (reverse res))            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
