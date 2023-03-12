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


;; 4.1.3 Evaluator Data Structures

;; Testing of predicates
(define false #f)
(define true #t)
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Representing procedures
;; (apply-primitive-procedure <proc> <args>)
;; applies the given primitive procedure to the argument values in the list <args> and returns the result of the application.
;; (primitive-procedure? <proc>)
;; test whether <proc> is a primitive procedure
;; Compound procedures are constructed from parameters, procedure bodies and environments using the constructor make-procedure:
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p)  (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (tagged-list? lst with)
  (and (pair? lst) (eq? (car lst) with)))

;; Operations on Environments
;; (lookup-varialle-value <var> <env>)
;; returns the value that is bound to the symbol <var> in the environment <env>, or signals an error is the value is unbound
;; (extend-environment <variables> <values> <base-env>)
;; returns a new environment, consisting of a new frame in which the symbols in the list <variables> are bound to the coresponding elements in the list <values>, where the enclosing environment is <base-env>.
;; (define-variable! <var> <value> <env>)
;; adds to the first frame of the environment <env> a new binding that associates the variable <var> with the value <value>.
;;; (set-variable-value! <var> <value> <env>)
;; changes the binding of <var> in the environment <env> so that the variable is bound to the value <value>, or signals an error if the variable is unbound

;; To implement these operations we represent an environment as a list of frames. The enclosing environment of an environment is the cdr of the list. The empty environment is simply the empty list.
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Each frame of an environment is represented as a pair of lists: a list of the variables bound in that frame and a list of the associated values.

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; To extend an environment by a new frame that associates variables with values, we make a frame consisting of the list of variables and the list of values, and we adjoin this to the environment.We signal an error if the number of variables doesn't match the number of values.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;; To look up a variable in an environmne, we scan the list of variables in the first frame. If we find the desired variable, we return the coresponding element in the list of values. If we don't find the variable in the current frame, we search the enclosing environment, and so on. If we search the empty environment, we signal an "unbound variable" error.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
       ((null? vars)
	(env-loop (enclosing-environment env)))
       ((eq? var (car vars)) (car vals))
       (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; To set a variable to a new value in a specified environment, we scan for the variable, just as in lookup-variable-value, and change the corresponding value when we find it.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
       ((null? vars)
	(env-loop (enclosing-environment env)))
       ((eq? var (car vars))
	(set-car! vals val))
       (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; To define a variable, we search the first frame for a binding for the variable, and change the binding if it exists (just as in set-variable-value!). If no such binding exists, we adjoin one to the first frame.
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond
       ((null? vars)
	(add-binding-to-frame! var val frame))
       ((eq? var (car vars))
	(set-car! vals var))
       (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;; 4.11: Instead of representing a frame as a pair of lists, we can represent a fram as a list of bindings, where each binding is name-value pair. Rewrite the environment operations to use this alternative representation.
(define (make-frame variables values)
  (if (= (length variables) (length values))
      (map cons variables values)
      (error "Different lengths: " variables values)))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame))
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((res (assoc var (frame-variables (first-frame env)))))
	  (if res
	      (cdr res)
	      (env-loop (enclosing-environment env))))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable: SET!" var)
	(let ((res (assoc var (frame-variables (first-frame env)))))
	  (if res
	      (set-cdr! res val)
	      (env-loop (enclosing-environment env))))))
  (env-loop env))
(define (define-variable! var val env)
  (let* ((frame (first-frame env))
	(res (assoc var (frame-variables frame))))
    (if res
	(set-cdr! res val)
	(add-binding-to-frame! var val frame))))

;; 4.12: The procedures set-variable-value!, define-variable! and lookup-variable-value can be expressed in terms of more abstract procedures for traversing the environment structure. Define abstraction that capture the common patterns and redefine the three procedures in terms of these abstractoins.
(define (env-loop var val env action)
  (define (scan vars vals)
    (cond
     ((and (eq? env the-empty-environment)
	   (or (eq? action 'set-variable-value!)
	       (eq? action 'define-variable!)))
      (error "Unbound variable: " var))
     ((null? vars)
      (if (or (eq? action 'set-variable-value!)
	      (eq? action 'lookup-variable-value))
	  (env-loop var val (enclosing-environment env) action)
	  (add-binding-to-frame! var val (first-frame env))))
     ((eq? var (car vars))
      (if (or (eq? action 'define-variable!)
	      (eq? action 'set-variable-value!))
	  (set-car! vals val)
	  (car vars)))
     (else (scan (cdr vars) (cdr vals)))))
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
	  (frame-values frame))))
(define (lookup-variable-value var env)
  (env-loop var '() env 'lookup-variable-value))
(define (define-variable! var val env)
  (env-loop var val env 'define-variable!))
(define (set-variable-value! var val env)
  (env-loop var val env 'set-variable-value!))

;; 4.13: Scheme allows us to create new bindings for variables by means of define, but provides no way to get rid of bindings. Implement for the evaluator a special form make-unbound! that removes the bidings of a given symbol from the environment in which the make-unbound! expresion is evaluated. This problem is not completely specified. For example, should we remove only
(define (make-unbound! var env)
  (let* ((frame (first-frame env))
	 (vars (frame-variables frame))
	 (vals (frame-values frame)))
    (define (scan vars vals new-vars new-vals)
      (cond ((null? vars)
	     (error "Variable not found: " var))
	    ((eq? var (car vars))
	     (set! frame
		   (cons
		    (append new-vars (cdr vars))
		    (append new-vals (cdr vals)))))
	    (else
	     (scan (cdr vars) (cdr vals)
		   (cons (car vars) new-vars)
		   (cons (car vals) new-vals)))))
    (scan vars vals '() '())))

;; 4.21: Amanzingly, Louis's intiition in Exercise 4.20 is correct. It is indeed possible to specify recursive procedures without using letrc (or even define), althought the method for accomplishing this is much more subtle than Louis imagined. The following expressino coputes 10 factorial by applying a recursive factorial procedure:
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1)
		       1
		       (* k (ft ft (- k 1)))))))
 10)

;; a. Check (by evaluating the expression) that this really does computes factorials. Devise an analogous expression for computing Fibonacci numbers.
((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (ft k) (cond ((= k 0) 1)
			 ((= k 1) 1)
			 (else
			  (+ (ft ft (- k 1))
			     (ft ft (- k 2))))))))
 10)

;; b. Consider the following procedure, which includes mutually recursive internal definitions:
(define (f x)
  (define (even? n)
    (if (= n 0) true (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false (even? (- n 1))))
  (even? x))
;; Fill in the missing expressions to complete an alternative definition of f, which uses neighter internal definitions nor letrec:
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;;;;;;;;;;;;;;;;;;;;
;; Playing around ;;
;;;;;;;;;;;;;;;;;;;;
;; y-combinator
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))
(define fact-once
  (lambda (f)
    (lambda (n)
      (if (= n 0)
	  1
	  (* n (f (- n 1)))))))
(define factorial (Y fact-once))
(factorial 20)

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define my-counter
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))
(my-counter)

(define (make-counter)
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))
(define make-counter
  (lambda ()
    (let ((counter 0))
      (lambda ()
	(set! counter (+ counter 1))
	counter))))
(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c2)
;;;;;;;;;;
;; Done ;;
;;;;;;;;;;

;; 4.1.7 Separating Syntactic Analysis from Execution

;; With the separation into analysis and execution, eval now becomes
(define (my-eval exp env) ((analyze exp) env))

;; The procedure that we dispatch performs only analysis, not full evaluation:
(define (analyze exp)
  (cond (self-evaluating? exp)
	(analyze-self-evaluating exp)
	((quoated? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((definition? exp)
	 (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp)
	 (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((application? exp)
	 (analyze-application exp))
	(else
	 (error "Unknown expression type: ANALYZE" exp))))

;; Here is the simplest analysis procedure, which handles selfevaluating expressions. It returns an execution procedure that ignores its environment argument and just returns the expression 
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;; For a quoted expression, we can gain a little efficiency by extracting the text of the quotation only once, in the analysis phase, rathen than in the execution phase
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))a
