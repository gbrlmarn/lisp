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
