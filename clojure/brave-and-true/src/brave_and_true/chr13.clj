(ns brave-and-true.chr13)

;; conj is polymorphic because it has different implementation based of different input. It works with list, vectors, sets, maps...you name it
(conj '(2 3 4) 1)
(conj [1 2 3] 4)
(conj #{1 2 3 4} 5)
(conj {:one 1 :two 2} {:three 3})

;; Polymorphic behaviour in Clojure is achived by the use of Multimethods, Records and Types

;; Multimethods
;; A dispacher is used that determines with method to use based on the arguments you provide
;; (ns were-creatures)
(ns were-creatures)
(defmulti full-moon-behavior
  (fn [were-creature] (:were-type were-creature)))
(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))
(defmethod full-moon-behavior :human
  [were-creature]
  (str (:name were-creature) " will save other humans"))

(full-moon-behavior {:were-type :human
                     :name "Jimmy"})
(full-moon-behavior {:were-type :wolf
                     :name "Jonny"})







