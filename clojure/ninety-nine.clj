;; Ninety-Nine Lisp Problems

(defn my-last [lst]
  ;; 1) Find the last box of a list
  (if (= (rest lst) '())
    (first lst)
    (my-last (rest lst))))
(my-last '(a b c d))


(defn my-but-last [lst]
  ;; 2) Find the last but one box of a list
  (if (= (count lst) 2)
    lst
    (my-but-last (rest lst))))
(my-but-last '(a b c d))


(defn element-at [lst pos]
  ;; 3)Find the K'th element of a list
  (if (= pos 1)
    (first lst)
    (element-at (rest lst) (- pos 1))))
(element-at '(a b c d) 3)


(defn length [lst]
  ;; 4)Find the number of elements in a list
  (reduce
   (fn [count _] (inc count))
   0 lst))
(length '(a b c d))

(defn reverse [lst]
  ;; 5) Reverse a list
  (reduce conj '() lst))
(reverse '(a b c d))
;; Playing around 
(conj '(c d) '(a b))
(conj nil '(a b))
(conj '(a b) nil)
(= (cons '(a b) nil)
   (conj nil '(a b)))

(defn palindrome [lst]
  ;; 6) find out whether a list is a palindrome
  (= lst (reverse lst)))
(palindrome '(x a m a x))
