(ns brave-and-true.chr11
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!!
                     go chan buffer close! thread
                     alts! alts!! timeout]]))

;; Mastering concurrent progresses with core.async

(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")

;; Putting something on a channel or consuming it are blocking operation. If a put something on a channel, I wait until that something is consumed.

;; Buffered channel remove the blocking part, by storing the number of values on the channel. If the values are reached...the next operation that will put a value on the channel must wait until another value is consumed...
(def echo-buffer (chan 2))
(go (println (<! echo-buffer)))
(>!! echo-buffer "ketchup")
;; We can put 2 ketchup-us without blocking :D
;; Also we can use a sliding buffer, which drops older values to make room for the newer ones or the other way around, drop newer values, until older ones are consumed 


(def hi-chan (chan))

(doseq [n (range 10)]
  (go (>! hi-chan (str "hellow " n))))
(doseq [n (range 10)]
  (go (println (<! hi-chan))))
;; There are two types of waiting: blocking and parking
;; Blocking a thread stops execution until the task is completed(ex: I/O operations)
;; Parking frees the thread so it can keep doing work

;; When you definitely need to use blocking instead of parking, the function 'thread' is used
(thread (println (<!! echo-chan)))
(>!! echo-chan "mustard")
;; 'thread' is exactly like 'future'. It creates a new thread and executes the process there. But unlike 'future' were you can deference the object that is returned, 'thread' returns a channel. When 'thread' process stops, the returned value is pute on the channel that 'thread' returns
(let [t (thread "chilli")]
  (<!! t))

;; Dog Machine Process You've Been Longing For
(defn hot-dog-machine
  []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

(let [[in out] (hot-dog-machine)]
  (>!! in "pocket lint")
  (<!! out))

;; Limited hot dogs in the machine
(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                (do (>! out "hot dog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(let [[in out] (hot-dog-machine-v2 2)]
  (>!! in "pocket lint")
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out)))


;; Transformations using multiple chanels
(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum"))

;; alts!!
(defn upload
  [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (upload "serious.jpg" c1)
  (upload "fun.jpg" c2)
  (upload "sassy.jpg" c3)
  (let [[headshot channel] (alts!! [c1 c2 c3])]
    (println "Sending headshot notification for"
            headshot)))
;; With the help of 'alts!!' the first result that appear on any channle is spit out :D
;; Also 'alts!!' permits to put a timeout. If the timeout is reached, 'alts!!' terminate
(let [c1 (chan)]
  (upload "serious.jpg" c1)
  (let [[headshot channel]
        (alts!! [c1 (timeout 20)])]
    (if headshot
      (println "Sending headshot notification for" headshot)
      (println "Time out!"))))

;;
(let [c1 (chan)
      c2 (chan)]
  (go (<! c2))
  (let [[value channel]
        (alts!! [c1 [c2 "put!"]])]
    (println value)
    (= channel c2)))

;; 'alts!!' is blocking while 'alts!' is parking
(defn append-to-file
  "Write a string to the end of a file"
  [filename s]
  (spit filename s :append true))

(defn format-quote
  "Delineate the beginning and end of a quote because it's convenient"
  [quote]
  (str "=== BEGIN QUOTE === \n"
       quote
       "=== END QUOTE \n\n"))

(defn random-quote
  "Retrive random quote and format it"
  []
  (format-quote
   (slurp "https://www.braveclojure.com/random-quote" )))

(defn snag-quotes
  [filename num-quotes]
  (let [c (chan)]
    (go
      (while true
        (append-to-file filename (<! c))))
    (dotimes [n num-quotes]
      (go (>! c (random-quote))))))
