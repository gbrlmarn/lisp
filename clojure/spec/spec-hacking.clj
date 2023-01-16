;; Messing around with spec library
;; to understand it better :D
;; All code is evaluated in emacs using
;; cider package for repl integration


(require '[clojure.spec.alpha :as s])

(s/conform even? 1000)
(s/valid? even? 10)
(s/valid? #(> % 5) 10)
(s/valid? #(< % 5) 10)

(import java.util.Date)
(s/valid? inst? (Date.))

(s/valid? #{:club :diamound :heart :spade}
          :spade)


(s/def :deck/suit #{:club :diamond :heart :spade})
(s/def :order/date inst?)

(s/valid? :order/date (Date.))
(s/conform :deck/suit :club)

;; Composing predicates

;; and spec
(s/def :num/big-even
  (s/and int? even? #(> % 1000)))
(s/valid? :num/big-even :foo)
(s/valid? :num/big-even 10)
(s/valid? :num/big-even 1002)

;; or spec
(s/def :domain/name-or-id
  (s/or :name string?
        :id int?))
(s/valid? :domain/name-or-id "abc")
(s/valid? :domain/name-or-id 10)
(s/valid? :domain/name-or-id :foo)

;; conform returns the tag value and value
(s/conform :domain/name-or-id "abc")
(s/conform :domain/name-or-id 10)
(s/conform :domain/name-or-id :foo)

;; how to provide nil value
(s/valid? string? nil)
(s/valid? (s/nilable string?) nil)

;; explain show why a value
;; does not confom to a spec
(s/explain :deck/suit 42)
(s/explain :domain/name-or-id 10)
(s/explain-data :domain/name-or-id :foo)

;; Entity maps
;; From what I understand spec facilitate
;; to know what value is in a map at the
;; key level, not at the value level :D
(def email-regex
  #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def :acct/email-type
       (s/and string?
              #(re-matches email-regex %)))

(s/def :acct/acctid int?)
(s/def :acct/first-name string?)
(s/def :acct/last-name string?)
(s/def :acct/email :acct/email-type)

(s/def :acct/person
  (s/keys :req [:acct/first-name
                :acct/last-name
                :acct/email]

          :opt [:acct/phone]))

(s/valid? :acct/person
          {:acct/first-name "Bugs"
           :acct/last-name "Bunny"
           :acct/email "bugs@looney.com"})

(s/valid? :acct/person
          {:acct/first-name "Maestro"})

(s/explain :acct/person
           {:acct/first-name "Maestro"
            :acct/last-name "Kimpembe"
            :acct/email "n/a"})

;; req-un and opt-un
;; require and optional unqualified
(s/def :unq/person
  (s/keys :req-un [:acct/first-name
                   :acct/last-name
                   :acct/email]
          :opt-un [:acct/phone]))

(s/conform :unq/person
           {:first-name "Bugs"
            :last-name "Bunny"
            :email "bugs@looney.com"})

(s/explain :unq/person
           {:first-name "Paul"
            :last-name "Pogba"
            :email "n/a"})

(s/explain :unq/person
           {:first-name "Maestro"})

;; Validate record attributes
;; using unqualified keys
(defrecord Person
    [first-name last-name email phone])

(s/explain :unq/person
           (->Person "Bugs" nil nil nil))

(s/conform :unq/person
           (->Person "Maestro"
                     "Kimpembe"
                     "maestro@fr.com"
                     nil))

(s/def :my.config/port number?)
(s/def :my.config/host string?)
(s/def :my.config/id keyword?)
(s/def :my.config/server
  (s/keys* :req [:my.config/id
                :my.config/host]
          :opt [:my.config/port]))
(s/conform :my.config/server
           [:my.config/id :s1
            :my.config/host "here.com"
            :my.config/port 5555])

(s/def :animal/kind string?)
(s/def :animal/says string?)
(s/def :animal/common
  (s/keys :req [:animal/kind
                :animal/says]))
(s/def :dog/tail? boolean?)
(s/def :dog/breed string?)
(s/def :animal/dog
  (s/merge :animal/common
           (s/keys :req [:dog/tail?
                         :dog/breed])))
(s/valid? :animal/dog
          {:animal/kind "dog"
           :animal/says "woof"
           :dog/tail? true
           :dog/breed "golden"})

;; multi-spec

;; definition
(s/def :event/type keyword?)
(s/def :event/timestamp int?)
(s/def :search/url string?)
(s/def :error/message string?)
(s/def :error/code int?)

(defmulti event-type :event/type)
(defmethod event-type :event/search [_]
  (s/keys :req [:event/type
                :event/timestamp
                :search/url]))
(defmethod event-type :event/error [_]
  (s/keys :req [:event/type
                :event/timestamp
                :error/message
                :error/code]))

;; declaration and testing
(s/def :event/event
  (s/multi-spec event-type :event/type))

(s/valid? :event/event
          {:event/type :event/search
           :event/timestamp 239487
           :search/url "https://clojure.org"})

(s/valid? :event/event
          {:event/type :event/error
           :event/timestamp 23497
           :error/message "Not found"
           :error/code 403})

(s/explain :event/event
           {:event/type :event/restart})
(s/explain :event/event
           {:event/type :event/search
            :search/url "here.there"
            :event/timestamp 23})

;; Collections

(s/conform (s/coll-of keyword?)
           [:a :b :c])
(s/conform (s/coll-of number?)
           #{5 10 2})
(s/conform (s/coll-of number?)
           [1 2 3])

(s/def :ex/vnum3
  (s/coll-of number?
             :kind vector?
             :count 3
             :distinct true
             :into #{}))
(s/conform :ex/vnum3 [1 2 3])
(s/explain :ex/vnum3 #{1 2 3})
(s/explain :ex/vnum3 [1 1 1])
(s/explain :ex/vnum3 [1 2 :a])

(s/def :geom/point
  (s/tuple double? double? double? ))
(s/conform :geom/point [1.5 2.5 -0.5])
(s/conform :geom/point [1 2.5 3.5])

(s/def :game/scores (s/map-of string? int?))
(s/conform :game/scores
           {"Alice" 1000
            "Bob" 1001})

;; Sequences
;; cat concatenation of predicates/patterns
(s/def :cook/ingredient
  (s/cat :quantity number?
         :unit keyword?))
(s/conform :cook/ingredient [2 :teaspoon])

;; string instead of keyword
(s/explain :cook/ingredient [11 "peaches"])

;; leave one element behind
(s/explain :cook/ingredient [12])

;; * -> 0 or more
;; + -> 1 or more
;; ? -> 0 or 1

(s/def :ex/seq-of-keywords
  (s/* keyword?))
(s/conform :ex/seq-of-keywords
           [:a :b :c])
(s/explain :ex/seq-of-keywords [10 20])

(s/def :ex/odds-then-maybe-even
  (s/cat :odds (s/+ odd?)
         :even (s/? even?)))
(s/conform :ex/odds-then-maybe-even
           [1 3 5 100])
(s/conform :ex/odds-then-maybe-even [13])
(s/conform :ex/odds-then-maybe-even [10])

(s/def :ex/opts
  (s/* (s/cat :opt keyword?
              :val boolean?)))
(s/conform :ex/opts
           [:silent? false
            :verbose true])

;; alt -> choice among alternatives
;; predicates/patterns

(s/def :ex/config
  (s/* (s/cat :prop string?
              :val (s/alt :s string?
                          :b boolean?))))
(s/conform :ex/config
           ["-server" "foo"
            "-verbose" true
            "-user" "Joe"])
(s/describe :ex/seq-of-keywords)
(s/describe :ex/odds-then-maybe-even)

(s/def :ex/even-strings
       (s/& (s/* string?)
            #(even? (count %))))
(s/valid? :ex/even-strings ["a"])
(s/valid? :ex/even-strings ["a" "b"])
(s/valid? :ex/even-strings ["a" "b" "c"])
(s/valid? :ex/even-strings ["a" "b" "c" "d"])

(s/def :ex/nested
  (s/cat :names-kw #{:names}
         :names (s/spec (s/* string?))
         :nums-kw #{:nums}
         :nums (s/spec (s/* number?))))
(s/conform :ex/nested
           [:names ["a" "b"]
            :nums [1 2 3]])
(s/def :ex/unnested
  (s/cat :names-kw #{:names}
         :names (s/* string?)
         :nums-kw #{:nums}
         :nums (s/* number?)))
(s/conform :ex/unnested
           [:names "a" "b"
            :nums 1 2 3])

;; Using spec for validation
(defn person-name
  [person]
  {:pre [(s/valid? :acct/person person)]
   :post [(s/valid? string? %)]}
  (str (:acct/first-name person)
       " "
       (:acct/last-name person)))
(person-name 42)

(person-name {:acct/first-name "Maestro"
              :acct/last-name "Kimpembe"
              :acct/email "maestro@fr.com"})

(defn person-name
  [person]
  (let [p (s/assert :acct/person person)]
    (str (:acct/first-name p)
         " "
         (:acct/last-name p))))
(s/check-asserts true)
(person-name 100)
(person-name {:acct/first-name "Maestro"
              :acct/last-name "Samuel"
              :acct/email "umtiti@allez.fr"})

(defn- set-config [prop val]
  (println "set" prop val))

(defn configure [input]
  (let [parsed (s/conform :ex/config input)]
    (if (s/invalid? parsed)
      (throw (ex-info "Invalid input"
                      (s/explain-data :ex/config input)))

      (for [{prop :prop [_ val] :val} parsed]
        (set-config (subs prop 1) val)))))

(configure ["-server" "foo"
            "-verbose" true
            "-user" "joe"])

;; Spec'ing functions
(defn ranged-rand
  "Returns random int in range start end"
  [start end]
  (+ start (long (rand (- end start)))))
(ranged-rand 10 20)
;; Specification for this function
(s/fdef ranged-rand
  :args (s/and (s/cat :start int?
                      :end int?)
               #(< (:start %) (:end %)))
  :ret int?
  :fn (s/and
       #(>= (:ret %) (-> % :args :start))
       #(< (:ret %) (-> % :args :end))))
(doc ranged-rand)

(defn adder [x] #(+ x %))

(s/fdef adder
  :args (s/cat :x number?)
  :ret (s/fspec :args (s/cat :y number?)
                :ret number?)
  :fn #(= (-> % :args :x) ((:ret %) 0)))
((adder 4) 5)

;; Macros receive code and
;; produce code. code --> code
(s/fdef clojure.core/declare
  :args (s/cat :names (s/* simple-symbol?))
  :ret any?)
(declare 100)

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

(defn deal [game] ....)

(s/fdef deal
  :args (s/cat :game :game/game)
  :ret :game/game
  :fn #(= (total-cards (-> % :args :game))
          (total-cards (-> % :ret))))
