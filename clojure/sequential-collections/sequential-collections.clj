;; Vectors
(get ["abc" false 99] 0)
(get ["abc" false 99] 1)
(get ["abc" false 99] 14)
(count [1 2 3])
(vector 1 2 3)

;; Adding elements
(conj [1 2 3] 4 5 6)

;; Immutability
(def v [1 2 3])
(conj v 4 5 6)
v

;; Lists
(def cards '(10 :ace :jack 9))
(first cards)
(rest cards)
(conj cards :queen)

;; Lists as stacks
(def stack '(:a :b :c))
(peek stack)
(pop stack)
