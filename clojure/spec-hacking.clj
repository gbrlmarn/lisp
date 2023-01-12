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
