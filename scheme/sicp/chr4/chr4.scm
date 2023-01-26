;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metalinguistic Abstraction   ;;
;; SICP Chapter 4               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "the most fundamental idea in
;; programming:
;; The evaluator, which determines the
;; meaning of expresions in a programming
;; language, is just another program."


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
