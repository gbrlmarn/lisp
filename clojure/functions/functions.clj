(defn greet [name] (str "Hello, " name))
(greet "Miki")

;; Multiple implementation of a function
(defn messenger
  ([] (messenger "Hello world!"))
  ([msg] (println msg)))

;; Variable parameters
(defn hello [greeting & who]
  (println greeting who))
(hello "Hei" "mister" ", how" " are" " you")

;; Anonymous Functions
;; params
;; -----------
((fn [message]
   ;; body            agument
   ;; --------------  ----------------
   (println message)) "Hello Clojure!")

;; defn vs fn
(defn greet [name] (str "Hello, " name))
;; equivalent
(def greet (fn [name] (str "Hello, " name)))

;; Anonymous function syntax
(fn [x] (+ 6 x))
;; equivalent
#(+ 6 %)

(fn [x y] (+ x y))
;; equivalent
#(+ %1 %2)

(fn [x y & zs] (println x y zs))
;; equivalent
#(println %1 %2 %&)

;; When you need an anonymous function that
;; takes a vector don't do it this way
#([%])
;; equivalent to
(fn [x] ([x]))

;; Insted do it this way
#(vector %)
;; equivalent to
(fn [x] (vector x))


;; Test your knowledge
;; Url: https://clojure.org/guides/learn/functions
;; 1)
(defn greet [] (println "Hello"))
(greet)

;; 2)
(def greet (fn [x] (println "Hello " x)))
(greet "Mister")
(def greet #(println "Hello " %))
(greet "Maestro")

;; 3)
(defn greeting
  ([] "Hello, World!")
  ([x] (str "Hello, " x "!"))
  ([x y] (str x ", " y "!")))

(assert (= "Hello, World!" (greeting)))
(assert (= "Hello, Clojure!"
           (greeting "Clojure")))
(assert (= "Good morning, Clojure!"
           (greeting "Good morning" "Clojure")))

;; 4)
(defn do-nothing [x] x)
(do-nothing 3)
(source identity)
(= (source identity) (source do-nothing))

;; 5)
(defn always-thing [& y] 100)
(always-thing 2 34 5 6 5)
(always-thing)

;; 6)
(defn make-thingy [x]
  (fn [& y] x))
((make-thingy 100) 3 5 8 6 39)
;; Tests
(let [n (rand-int Integer/MAX_VALUE)
      f (make-thingy n)]
  (assert (= n (f)))
  (assert (= n (f 123)))
  (assert (= n (apply f 123 (range)))))

;; 7)
(defn triplicate [f]
  (repeatedly 3 f))
(defn tripliate [f]
  (do (f) (f) (f)))
(defn f [] "Omg")
(triplicate f)

;; 8)
(defn opposite [f]
  (fn [& args]
    (not(map f args))))
((opposite #(+ 1 %)) 3)

;; 9)
(defn triplicate2 [f & args]
  (triplicate #(apply f args)))
(triplicate2 #(+ % 1) 2 3 4)

;; 10)
(Math/cos Math/PI)
(+ (Math/pow (Math/sin 0.5) 2)
   (Math/pow (Math/cos 0.5) 2))


