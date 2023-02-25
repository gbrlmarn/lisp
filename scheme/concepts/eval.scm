;; From url: https://archives.evergreen.edu/webpages/curricular/2000-2001/fofc00/eval.html

;;;;;;;;;;
;; Eval ;;
;;;;;;;;;;

;; Eval can be defined in terms of himself =>
;; metacircular definition

;; Primitive expressions
;;   lambda expression
;;     => ((params body) env)
;;     => applicable procedure

;; Combinations
;;   (eval (operator operands))
;;      => (apply procedure args)

;; Definition of eval:
(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp)
    (lookup-variable-value exp env))
   ((quoted? exp) (text-of-quotation exp))
   ((assignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((lambda? exp)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ((cond? exp) (eval (cond->if exp) env))
   ((application? exp)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type - EVAL" exp))))

;;;;;;;;;;;
;; Apply ;;
;;;;;;;;;;;

;; Apply takes two arguments
;; (apply procedure args)

;; (list-of-values exps env)
;; => list-of-arguments
;; => (apply procedure list-of-arguments)
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cond (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps)
			    env))))

;; eval-if

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent ex) env)
      (eval (if-alternative exp) env)))
