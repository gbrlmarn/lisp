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
