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
