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

