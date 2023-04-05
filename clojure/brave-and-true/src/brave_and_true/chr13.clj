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
  (fn [were-creature]
    (:were-type were-creature)))
(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature)
       " will howl and murder"))

(defmethod full-moon-behavior :human
  [were-creature]
  (str (:name were-creature)
       " will save other humans"))

(defmethod full-moon-behavior nil
  [were-creature]
  (str (:name were-creature)
       " will do nothing..."))

(defmethod full-moon-behavior :default
  [were-creature]
  (str (:name were-creature)
       " will do what is needed"))

(full-moon-behavior {:were-type :human
                     :name "Jimmy"})
(full-moon-behavior {:were-type :wolf
                     :name "Jonny"})
(full-moon-behavior {:were-type nil
                     :name "Nobody"})
(full-moon-behavior {:were-type :engineer
                     :name "Gabriel"})

;; Use method in another namespace and extend it
(ns random-namespace
  (:require [were-creatures]))
(defmethod
  were-creatures/full-moon-behavior :vampire
  [were-creature]
  (str (:name were-creature)
       " will steal your blood"))
(were-creatures/full-moon-behavior
 {:were-type :vampire
  :name "Merlin"})

;; Multimethod that takes two inputs
(ns user)
(defmulti types
  (fn [x y] [(class x) (class y)]))
(defmethod types
  [java.lang.String java.lang.String]
  [x y]
  "Two strings!")

(defmethod types
  [java.lang.Long java.lang.Long]
  [x y]
  "Two numbers...")

(types "String 1" "String 2")
(types 2 3)







