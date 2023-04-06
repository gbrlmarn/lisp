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


;; Protocols
;; Although it's posible to do type dispatch with multimethods, protocols are optimized for type dispatch
(ns data-languages)
(defprotocol Languages
  "Different languages"
  (native-language [x] "My native language")
  (languages [x] [x y] "Other languages"))
(extend-type java.lang.String
  Languages
  (native-language [x]
    (str x " is my native language"))
  (languages
    ([x]
     (str x " is a language that I know"))
    ([x y]
     (str x " and " y " are languages that I know."))))

(native-language "Romanian")
(languages "Romanian")
(languages "Romanian" "English")

;; Every type in Java(also Clojure) is a descendent from 'java.lang.Object'. If we want to extend a protocol in the generales way possible, we extend it from the java.lang.Object
(extend-type java.lang.Object
  Languages
  (native-language [x]
    (str "I don't think so..."))
  (languages
    ([x] (str "I don't think so"))
    ([x y] (str "Nah..."))))

(native-language "Romanian")
(languages 3)
(languages 1 2)


;; We can extend a protocol more easily by the use of extend-protocol
(extend-protocol Languages
  java.lang.String
  (native-language [x]
    (str x " is my native language"))
  (languages
    ([x] (str x " is a language that I know"))
    ([x y] (str x " and " y " are languages that I know")))
  java.lang.Object
  (native-language [x]
    (str "I don't think so"))
  (languages
    ([x] (str "No no no..."))
    ([x y] (str "Definetely no......"))))

(native-language "Romanian")
(native-language 3)
(languages "Romanian")
(languages 1)
(languages "Romanian" "English")
(languages 1 2)

;; Protocols belong to their namespace where are created

;; Records
;; Are map-like and can be extended with the likes of 'Protocols'
(ns persons)
(defrecord Person [name language])

;; A Java Object is created
;; Records are Java Object undercover
(Person. "Gabriel" "Romanian")

;; Both ->RecordName & map->RecordName are created automatically and are function for the use of record creationg(Constructor bleach...) 
(->Person "Gabriel" "Romanian")
(map->Person
 {:name "Gabriel"
  :language "Romanian"})

;; Records can be imported in another namespace by using import
(ns young-persons
  (:import [persons Person]))
(Person. "Gabriel" "Romanian")
(def me (Person. "Gabriel" "Romanian"))

(.name me)
(:name me)
(get me :name)

(= me (Person. "Gabriel" "Romanian"))
(= me {:name "Merlin"
       :language "Spellish"})

(assoc me :name "Merlin")
(dissoc me :name)

;; Here is how a protocol is extend when defining a record
(ns persons-talk)

(defprotocol Talk
  (greetings [x]))

(defrecord Person [name language]
  Talk
  (greetings [x]
    (str name " will greet you in " language)))

(greetings (Person. "Gabriel" "Romanian"))
(greetings (map->Person
           {:name "Gabriel"
            :language "English"}))

;; Records has better access performance than Maps
;; If you want to create a protocol...you'll need to create a record

;; Exercises
;; 1. Extend the full-moon-behavior multimethod to add behavior for your own kind of were-creature
(defmulti full-moon-behavior
  (fn [were-creature]
    (:were-type were-creature)))
(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature)
       " will howl and murder"))
(full-moon-behavior {:were-type :wolf
                     :name "Spike"})

(defmethod full-moon-behavior :vampire
  [were-creature]
  (str (:name were-creature)
       " will hunt everything"))
(full-moon-behavior
 {:were-type :vampire
  :name "Merlin"})

;; 2. Create a WereSimmons record type, and then extend the WereCreature protocol.
(defprotocol WereCreature
  (full-moon-behavior [x]))

(defrecord WereWolf [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will howl and murder")))

(full-moon-behavior
 (map->WereWolf {:name "Spike"
                 :title "Kind"}))


(defrecord WereSimmons [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name " will protect humans")))

(full-moon-behavior
 (map->WereSimmons {:name "Rambo"
                    :title "Saviour"}))

;; 3. Create your own protocol, and then extend it using extend-type and extend-protocol
(ns protocols-extensions)

(defprotocol Cardistry
  "Different cardistry moves"
  (show-one-move [x] "Novice")
  (show-two-moves [x y] "Medium")
  (show-three-moves [x y z] "Expert"))

(extend-type java.lang.String
  Cardistry
  (show-one-move [x] (str x " is my first move"))
  (show-two-moves [x y]
    (str "I know " x " and " y))
  (show-three-moves [x y z]
    (str "I'm an expert and I know "
         x " " y " and " z)))

(show-one-move "Charlier")
(show-two-moves "Revolution" "Sybil")
(show-three-moves "Pandora" "Jackson 5" "Tornado")

(extend-protocol Cardistry
  java.lang.Long
  (show-one-move [x]
    (str "Nah..."))
  (show-two-moves [x y]
    (str "I don't think so"))
  (show-three-moves [x y z]
    (str "You can do better...")))


(show-one-move 1)
(show-two-moves 1 2)
(show-three-moves 1 2 3)

;; 4. Create a role-playing game that implements behavior using multiple dispatch
