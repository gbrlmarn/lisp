(ns brave-and-true.chr10)

;; Clojure Metaphysics: Atoms, Refs, Vars, and Cuddble Zombies


;; Clojure's atom refrence type allows you to endow a succession of related values with an identity. Here's how you create one:
(def fred (atom {:cuddle-hunger-level 0                 :percent-deteriorated 0}))
@fred
;; When you dereference futures, delays and promises the operation might block until they get the value.
;; When you dereference a reference type, the operation is instant.
;; Each state is immutable
(let [zombie-state @fred]
  (if (>= (:percent-deteriorated zombie-state) 50)
    (future (println (:percent-deteriorated zombie-state)))))

;; Swap receives an atom and a function as arguments. It applies the function to the atom's current state to produce a new value, and then it updates the atom to refer to the new value which is returned.
(swap! fred
       (fn [current-state]
         (merge-with + current-state
                     {:cuddle-hunger-level 1})))

;; Unlike Ruby, it's not possible for fred to be in an inconsistent state, because you can update the hunger level and deterioration percentage at the same time, like this:
(swap! fred
       (fn [current-state]
         (merge-with + current-state
                     {:cuddle-hunger-level 1
                      :percent-deteriorated 1})))
;; Create a function that increses cuddle-hunger-level
(defn increase-cuddle-hunger-level
  [zombie-state increase-by]
  (merge-with + zombie-state
             {:cuddle-hunger-level increase-by}))

;; Here fred is not updated because swap is not called. Only the result is returned
(increase-cuddle-hunger-level @fred 10)

(swap! fred increase-cuddle-hunger-level 10)

;; Clojure built-in function for changing values
;; first argument is a map, second is a vector of keys for finding values that will be updated and third the operation done on the values
(update-in {:a {:b 3}} [:a :b] inc)

(update-in {:a {:b 3}} [:a :b] + 2)

;; Here is how we update the state of fred
(swap! fred update-in [:cuddle-hunger-level] + 10)

;; By using atoms you can retain past states. You can dereference an atom to retrive State1, and then update the aotm, creating State 2, and still make use of State 1
(let [num (atom 1)
      s1 @num]
  (swap! num inc)
  (println "State 1:" s1)
  (println "Current state:" @num))

;; swap! is thread-safe. It implements compare-and-set sematics, meaning it does the following internally:
;; 1. It reads the current state of the atom.
;; 2. It then applies the update function to the state
;; 3. Next, it checks whether the value it reads in step 1 is identical to the atom's current state
;; 4. If it is, then swap! updates the atom to refer to the result of step 2.
;; 5. If it isn't, then swap! retries going through the process agian with step 1.
(reset! fred {:cuddle-hunger-level 0
              :percent-deteriorated 0})

;; Watches and Validators
;; A watch is a function that takes four arguments: a key, the reference being watched, its previous state, and its new value.
(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now " sph)
        (println "This message brought to you courtesy of " key))
      (do
        (println "All's well with " key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: "
                 (:percent-deteriorated new-state))
        (println "SPH: " sph)))))

;; (add-watch ref key watch-fn)
(reset! fred {:cuddle-hunger-level 22
              :percent-deteriorated 2})
(add-watch fred :fred-shuffle-alert shuffle-alert)
(swap! fred update-in [:percent-deteriorated] + 1)
(swap! fred update-in [:cuddle-hunger-level] + 30)

;; Validators
;; They let you specifify what states are allowable for a reference. For example, here's a validator that you could use to ensure that a zombie's :percen-deteriorated is between 0 to 100
(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (and (>= percent-deteriorated 0)
       (<= percent-deteriorated 100)))

;; Validator takes only one argument. A reference that contains a validator can be change only if the validator returns true for the new value.
;; A validator can be attached during atom creation:
(def bobby
  (atom
   {:cuddle-hunger-level 0
    :percent-deteriorated 0}
   :validator percent-deteriorated-validator))
(swap! bobby update-in [:percent-deteriorated] + 200)
;; Also a messages can be set if the validator returns false.
(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (or (and (>= percent-deteriorated 0)
           (<= percent-deteriorated 100))
      (throw (IllegalStateException. "That's not mathy!"))))
(def bobby
  (atom
   {:cuddle-hunger-level 0
    :percent-deteriorated 0}
   :validator percent-deteriorated-validator))
(swap! bobby update-in [:percent-deteriorated] + 200)

;; Atoms are ideal for managing independent identities. For updating the state of more than one identity simultaneously 'refs' are used
(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair"
    "mulleted" "passive-aggresive" "striped"
    "polka-dotted" "athletic" "business"
    "power" "invisibile" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Createa an initial sock gnome state with no socks"
  [name]
  {:name name
   :socks #{}})

;; The gnome will have 0 socks and the dryers will have the rest of generated socks.
(def sock-gnome
  (ref (generate-sock-gnome "Barumpharumph")))
(def dryer
  (ref {:name "LG 1337"
        :socks (set (map #(sock-count % 2)
                         sock-varieties))}))
(:socks @dryer)

;; Define the transfer of socks from the dryer to gnome using alter.
(defn steal-sock
  [gnome dryer]
  (dosync
   (when-let [pair (some #(if (= (:count %) 2) %)
                         (:socks @dryer))]
     (let [updated-count
           (sock-count (:variety pair) 1)]
       (alter gnome update-in [:socks]
              conj updated-count)
       (alter dryer update-in [:socks]
              disj pair)
       (alter dryer update-in [:socks]
              conj updated-count)))))
(steal-sock sock-gnome dryer)

;; Find similar socks from gnome and dryer
(defn similar-socks
  [target-sock sock-set]
  (filter #(= (:variety %) (:variety target-sock)) sock-set))
(similar-socks (first (:socks @sock-gnome)) (:socks @dryer))

;; When a ref is altered the value isn't changed immediately
(def counter (ref 0))
(future
  (dosync
   (alter counter inc)
   (println @counter)
   (Thread/sleep 500)
   (alter counter inc)
   (println @counter)))
(Thread/sleep 250)
(println @counter)

;; commute
;; allows to update a ref's state withing a transaction, just like alter but its behavior at commit time is completely different. Commute doesn't force transaction retry.
;; safe commuting
(defn sleep-print-update
  [sleep-time thread-name update-fn]
  (fn [state]
    (Thread/sleep sleep-time)
    (println (str thread-name ": " state))
    (update-fn state)))
(def counter (ref 0))
(future (dosync (commute counter
                         (sleep-print-update
                          10 "Thread A" inc))))
(future (dosync (commute counter
                         (sleep-print-update
                          150 "Thread B" inc))))
@counter

;; unsafe commuting
(def receiver-a (ref #{}))
(def receiver-b (ref #{}))
(def giver (ref #{1}))
(do (future (dosync
             (let [gift (first @giver)]
               (Thread/sleep 10)
               (commute receiver-a conj gift)
               (commute giver disj gift))))
    (future (dosync
             (let [gift (first @giver)]
               (Thread/sleep 50)
               (commute receiver-b conj gift)
               (commute giver disj gift)))))
@receiver-a
@receiver-b
@giver

;; Dynamic Binding
(def ^:dynamic *notification-address* "dobby@elf.org")
;; The value can be changes temporarly by using binding
(binding [*notification-address* "test@elf.org"]
  *notification-address*)
*notification-address*

;; Bindings cand be stacked just as let
(binding [*notification-address* "tester-1@elf.org"]
  (println *notification-address*)
  (binding [*notification-address* "tester-2@elf.org"]
    (println *notification-address*))
  (println *notification-address*))

;; Dynamic Var Uses
(defn notify
  [message]
  (str "TO: " *notification-address* "\n"
       "MESSAGE: " message))
(notify "I fell.")

;; The function can be tested without spamming little dobby
(binding [*notification-address* "test@elf.org"]
  (notify "I feel."))
;; Why not define notification address at input and why use dynamic binding instead. Dynamic bindings are used to name a resource that is accessed by more functions. Clojure comes with a ton of built-in dynamics vars like *out*(standard output)
(binding [*out* (clojure.java.io/writer "print-output")]
  (println "A man who carries a cat by the tail learns somethings he can lean in no other way.
-- Mark Twain"))
(slurp "print-output")

;; Dynamic vars ar also good for configuration. For example the *print-length* allows to specify the number of elements println should print from a collection
(println ["Print" "all" "the" "things!"])
(binding [*print-length* 1]
  (println ["Print" "just" "one"]))

;; Conccurrency and Parallelism with pmap
;; each applicatoin of a function over an element using is done in a separate thread whe using pmap
(defn always-1 [] 1)
(take 5 (repeatedly always-1))
(take 5 (repeatedly (partial rand-int 10)))

;; Generate a sequence of 3000 random string each 7000 characters long. We'll compare 'map' and 'pmap' on this sequence
(def alphabet-length 26)

;; Vector of chars, A-Z0vtl
(def letters
  (mapv (comp str char (partial + 65))
        (range alphabet-length)))
letters

;; Return of random string of length
(defn random-string
  [length]
  (apply str
         (take length
               (repeatedly #(rand-nth letters)))))
(random-string 3)

(defn random-string-list
  [list-length string-length]
  (take list-length
        (repeatedly #(random-string string-length))))
(defn random-string-list
  [list-length string-length]
  (doall
   (take list-length
         (repeatedly (partial random-string
                              string-length)))))
(random-string-list 5 3)

(def orc-names (random-string-list 3000 7000))

;; dorun realizes the sequence but returns nil
(time (dorun (map clojure.string/lower-case orc-names)))
(time (dorun (pmap clojure.string/lower-case orc-names)))

;; the paralization of function application has a time overhead. If we generate more words but shorter, the serial version 'map' will be faster than 'pmap'
(def orc-names-abbrevs (random-string-list 20000 300))
(time (dorun (map clojure.string/lower-case orc-names-abbrevs)))
(time (dorun (pmap clojure.string/lower-case orc-names-abbrevs)))

;; The solution is to increase the ammount of work that is done by each thread. Instead of applying the function on one element per thread, we can have a thread apply the functoin on 2 or 3 elements, etc...
;; We use partitioning to divide our collection
(def numbers [1 2 3 4 5 6 7 8 9 10])
(partition-all 3 numbers)
;; and concat to restore the initial collection

