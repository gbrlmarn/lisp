(ns brave-and-true.chr9)

;; The sacred art of concurrent and parallel programming

;; Futures
;; We can define a task that can take place in another
;; thread without requiring the result immediately.
(future (Thread/sleep 4000)
        (println "I'll print after 4 seconds"))
(println "I'll print immediately")

(let [result (future (println "this prints once")
                     (+ 1 1))]
  (println "deref: " (deref result))
  (println "@: " @result))

;; If future longer than 10 => 5 else => 0
(deref (future (Thread/sleep 1000) 0) 10 5)

;; Delays
;; Delays allow to define a task without executing it
;; or require the result immediately.
(def jackson-5-delay
  (delay (let [message "Just call my name and I'll be there"]
           (println "First deref:" message)
           message)))
(force jackson-5-delay)
@jackson-5-delay

(def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])
(defn email-user
  [email-address]
  (println "Sending headshot notification to" email-address))
(defn upload-document
  "Needs to be implemented"
  [headshot]
  true)
(let [notify (delay (email-user "and-my-axe@gmail.com"))]
  (doseq [headshot gimli-headshots]
    (future (upload-document headshot)
            (force notify))))
;; (email-user "and-my-axe@gmail.com") is guarded by
;; the use of delay.

;; Promises
;; Alow you to express that you expect a result
;; without having to define the task that should
;; produce that result. Create a promise with
;; promise and deliver the result with deliver...daa...
(def my-promise (promise))
(deliver my-promise (+ 1 1))
(deref my-promise)
@my-promise

;; The result can be delivered only once
(def yak-butter-international
  {:store "Yak Butter International"
   :price 90
   :smoothness 90})
(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})
;; This is the butter that meets our requirements
(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, returns the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

;; Checking sychronously
(time (some (comp satisfactory? mock-api-call)
            [yak-butter-international
             butter-than-nothing
             baby-got-yak]))

;; This can be performed using multiple threads
;; if the computer has multiple cores
(time
 (let [butter-promise (promise)]
   (doseq [butter [yak-butter-international
                   butter-than-nothing
                   baby-got-yak]]
     (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
               (deliver butter-promise satisfactory-butter))))
   (println "And the winner is:" @butter-promise)))

;; If the promise cannot be dereferences we must include
;; a time-out.
(let [p (promise)]
  (deref p 100 "time out"))

;; Callbacks => a way of executing code asynchronously
;; after some other code finishes.
(let [ferengi-wisdom-promise (promise)]
  (future (println "Here's some Ferengi wisdom:"
                   @ferengi-wisdom-promise))
  (Thread/sleep 100)
  (deliver ferengi-wisdom-promise "Whisper your way to success."))

;; Rolling Your Own Queue
(defmacro wait
  "Sleep 'timeout' seconds before evaluating the body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

;; This code splits task into a concurrent portion and a serialized portion
(let [saying3 (promise)]
  (future (deliver saying3 (wait 100 "Cheerio!")))
  @(let [saying2 (promise)]
     (future (deliver saying2 (wait 400 "Pip pip!")))
     @(let [saying1 (promise)]
        (future (deliver saying1 (wait 200 "Ello, gov'na!")))
        (println @saying1)
        saying1)
     (println @saying2)
     saying2)
  (println @saying3)
  saying3)

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name
             ~concurrent ~serialized)))

(time @(-> (enqueue saying (wait 200 "'Ello, gov'na!")
                    (println @saying))
           (enqueue saying (wait 400 "Pip pip!")
                    (println @saying))
           (enqueue saying (wait 100 "Cheerio!")
                    (println @saying))))

;; Summary
;; Concurrent programming
;; Risks: reference cells, mutual exclusion and deadlock.
;; Mitigators: futures, delays and promises.
;; Futures => define a task and execute it immediately
;; Delays => define a task that doesn't get executed
;;           until later
;; Promises => express that you require a result without
;;             having to know the task that produces
;;             the result

;; Exercises
;; 1. Write a function that takes a string as an argument and searches for it on Bing and Google using the slurp function. Your function should return the HTML of the first page returned by the search.
;; 2. Update your function so it takes a seocnd argument consisting of the search engines to use
(def default-search-engines
  ["https://www.google.com/search?q%3D"
   "https://www.bing.com/search?q%3D"])
(defn search
  ([query] (search query default-search-engines))
  ([query search-engine]
   (let [result-promise (promise)]
     (doseq [engine search-engine]
       (future (deliver result-promise
                        (slurp (str engine query)))))
     @result-promise)))
(search "clojure")
(search "clojure")

;; 3. Create a new function that takes a search term and search engines as arguments, adn returns a vector of the URLs. fomr the first pages of search result from each search engine
(defn get-urls
  [source]
  (re-seq #"https?://[^\"]*" source))
(defn promised-request
  [query search-engines]
  (let [request-promise (promise)]
    (doseq [engine search-engines]
      (future (deliver request-promise
                       (slurp (str engine query)))))
    request-promise))

(deref (promised-request "clojure" default-search-engines))

(defn search
  [query search-engines]
  (vec (flatten
        (map #(get-urls (deref %))
             (map #(promised-request query %)
                  search-engines)))))
(search "clojure" default-search-engines)
