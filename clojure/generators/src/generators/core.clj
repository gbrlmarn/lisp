(ns generators.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(foo "Nic")

(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.gen.alpha :as gen])

(gen/generate (s/gen int?))
(gen/generate (s/gen nil?))
(gen/sample (s/gen string?))
(gen/sample
 (s/gen #{:club :diamond :heart :spade}))
(gen/sample
 (s/gen (s/cat :k keyword?
               :ns (s/+ number?))))

;; A game of cards
(def suit? #{:club :diamond :heart :spade})
(def rank? (into #{:jack :queen :king :ace} (range 1 10)))
(def deck (for [suit suit? rank rank?]
            [rank suit]))
(s/def :game/card (s/tuple rank? suit?))
(s/def :game/hand (s/* :game/card))

(s/def :game/name string?)
(s/def :game/score int?)
(s/def :game/player
  (s/keys :req [:game/name
                :game/score
                :game/hand]))

(s/def :game/players (s/* :game/player))
(s/def :game/deck (s/* :game/card))
(s/def :game/game
  (s/keys :req [:game/players
                :game/deck]))

(def kenny
  {:game/name "Kenny S"
   :game/score 100
   :game/hand []})
(s/valid? :game/player kenny)

(s/explain
 :game/game
 {:game/deck deck
  :game/players
  [{:game/name "Kenny S"
    :game/score 100
    :game/hand [[2 :club]]}]})

(defn total-cards
  [{:keys [:game/deck :game/players] :as game}]
  (apply + (count deck)
         (map #(-> % :game/hand count)
              players)))


(gen/generate (s/gen :game/player))
(gen/generate (s/gen :game/game))

;; Write a game to a file
(require '[clojure.java.io :as io])
(System/get)
(let [wrt (io/writer "./resources/a-game.txt")]
  (.write
   wrt
   (str (gen/generate (s/gen :game/game))))
  (.close wrt))
;; Read from that file
(let [rdr (io/reader "./resources/a-game.txt")]
  (println (.readLine rdr)))

;; Exercise

(s/exercise (s/cat :k keyword?
                   :ns (s/+ number?)) 5)
(s/exercise (s/or :k keyword?
                  :s string?
                  :n number?) 5)

;; Using s/ and Generators
(gen/generate (s/gen (s/and int? even? #(> % 0))))

(defn divisibile-by [n]
  #(zero? (mod % n)))
(gen/sample
 (s/gen (s/and int?
               #(> % 0)
               (divisibile-by 3))))

;; Generators that are to constrain
;; give an errror after 100 tries
(gen/sample
 (s/gen (s/and string?
               #(clojure.string/includes? % "hello"))))

;; Custom generators
(s/def :ex/kws
  (s/and keyword?
         #(= (namespace %) "my.domain")))
(s/valid? :ex/kws :my.domain/name)
(gen/sample (s/gen :ex/kws))

(def kw-gen
  (s/gen #{:my.domain/name
           :my.domain/occupation
           :my.domain/id}))
(def kw-gen
  #{:my.domain/name
   :my.domain/occupation
   :my.domain/id})
(s/def :ex/kws
  (s/with-gen
    (s/and keyword?
           #(= (namespace %) "my.domain"))
    #(s/gen kw-gen)))
(s/valid? :ex/kws :my.domain/name)
(gen/sample (s/gen :ex/kws))

(def kw-gen-2
  (gen/fmap #(keyword "my.domain" %)
            (gen/string-alphanumeric)))
(gen/sample kw-gen-2 5)

;; Remove empty string for generator
(def kw-gen3
  (gen/fmap
   #(keyword "my.domain" %)
   (gen/such-that
    #(not= % "")
    (gen/string-alphanumeric))))
(gen/sample kw-gen3 3)

(s/def :ex/hello
  (s/with-gen
    #(clojure.string/includes? % "hello")
    #(gen/fmap
      (fn [[s1 s2]] (str s1 "hello" s2))
      (gen/tuple
       (gen/string-alphanumeric)
       (gen/string-alphanumeric)))))
(gen/sample (s/gen :ex/hello))

;; Range Specs and Generators
(s/def :bowling/boll (s/int-in 0 11))
(gen/sample (s/gen :bowling/boll))
(s/def :dice/roll (s/int-in 1 7))
(gen/sample (s/gen :dice/roll))

(s/def :ex/the-aughts
  (s/inst-in #inst "2000" #inst "2010"))
(drop 50 (gen/sample (s/gen :ex/the-aughts) 55))

(s/def :ex/dubs
  (s/double-in :min -100.0
               :max 100.0
               :Nan? false?
               :infinite? false))
(s/valid? :ex/dubs 2.9)

(s/valid? :ex/dubs Double/POSITIVE_INFINITY)

(gen/sample (s/gen :ex/dubs))

;; Instrumentation and testing

;; Combining check and instrument
(require '[clojure.spec.test.alpha :as stest])

(defn invoke-service [service request]
  ;; Invokes remote service
  )


(defn run-query [service query]
  (let [{:svc/keys [result error]}
        (invoke-service service {:svc/query query})]
    (or result error)))
(s/def :svc/query string?)
(s/def :svc/request (s/keys :req [:svc/query]))
(s/def :svc/result (s/coll-of string?
                              :gen-max 3))
(s/def :svc/error int?)
(s/def :svc/response
  (s/or :ok (s/keys :req [:svc/result])
        :err (s/keys :req [:svc/error])))

(s/fdef invoke-service
  :args (s/cat :service any?
               :request :svc/request)
  :ret (s/or :ok :svc/result
             :err :svc/error))
(s/fdef run-query
  :args (s/cat :service any?
               :query string?)
  :ret (s/or :ok :svc/result
             :err :svc/error))
;; Behavior testing(behaviorist...)
(stest/instrument
 `invoke-service {:stub #{`invoke-service}})
(invoke-service nil {:svc/query "test"})
(invoke-service nil {:svc/query "test"})
(stest/summarize-results
 (stest/check `run-query))

;; The end...but still...
;;alot of work must to be done.



