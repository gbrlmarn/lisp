;; 10 Macros

;; 10.1 Eval
(eval '(+ 1 2 3))
(eval '(format t "Hello"))

(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

(coerce '(lambda (x) x) 'function)

;; 10.2 Macros
(defparameter x 3)
(defmacro nil! (x)
  (list 'setf x nil))
(nil! x)

(setf a 1 b 2)
`(a is ,a and b is ,b)

(defmacro nil! (x)
  `(setf ,x nil))
(setf lst '(a b c))
`(lst is ,lst)
`(lst elements are ,@lst)


(defmacro my-while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(let ((x 0))
  (my-while (< x 10)
	 (princ x)
	 (incf x)))

;; 10.4 Example: Quicksort
(defun quicksort (vec l r)
  (let ((i l)
	(j r)
	(p (svref vec (round (+ 1 r) 2))))
    (while (<= i j)
	   (while (< (svref vec i) p)
		  (inf i))
	   (while (> (svref vec j) p)
		  (decf j))
	   (when (<= i j)
	     (rotatef (svref vec i)
		      (svref vec j))
	     (incf i)
	     (decf j)))
    (if (> (- j l) 1)
	(quicksort vec l j))
    (if (> (- r i) 1)
	(quicksort vec i r)))
  vec)

(defmacro ntimes (n &rest body) ; wrong
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

(let ((x 10))
  (ntimes 5 (setf x (+ x 1)))
  x)

(defmacro ntimes (n &rest body)		; wrong
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
	 ((>= ,g ,n))
       ,@body)))

(let ((v 10))
  (ntimes (setf v (- v 1))
	  (princ ".")))
;; => .....
;; Should print
;; => ..........

(defmacro ntimes (n &rest body)		; correct
  (let ((g (gensym))
	(h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
	   ((>= ,g ,h))
	 ,@body))))

;; Macro expansion
(pprint (macroexpand-1 '
	 (cond (a b)
	       (c d e)
	       (t f))))
;; =>
;; (IF A
;;     B
;;     (IF C
;;         (PROGN D E)
;;         (THE T F)))

;; 10.7 Example: Macro Utilities
(defmacro for (var start stop &rest body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (+ 1 ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))
(for x 1 9 (princ x))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(avg 1 2 3 4 5 6 7)

(defmacro with-gensym (syms &body body)
  `(let ,(mapcar)))

;; Exercises

;; 1. If x is a, y is b, and z is (c d),
;; write backquoated expressions
;; containing only variables that yield
;; each of the following
;; (a) ((C D) A Z)
;; (b) (X B C D)
;; (c) ((C D A) Z)
(let ((x 'a)
      (y 'b)
      (z '(c d)))
  `((,z ,x z)
    (x ,y ,@z)
    ((,@z ,x) z)))

;; 2. Define if in term of cond.
(defmacro cif (test then else)
  `(cond
     (,test ,then)
     (t ,else)))

;; 3. Define a macro that takes a number
;; followed by one or more expressions,
;; and returns the value of the nth
;; expression:


