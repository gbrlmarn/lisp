(ns brave-and-true.boot-walkthrough.apendix)

(defn inc-str
  [rejects]
  {:pre [(set? rejects)]}
  (fn [x]
    (if (rejects x)
      (str "I don't like " x)
      (str x))))
(def increment-string
  (comp (inc-str #{2}) inc))
(increment-string 1)

;; Not call inc in the first place
(defn middleware
  [next-handler rejects]
  {:pre [(set? rejects)]}
  (fn [x]
    (if (= x 1)
      "I'm not going to do anything..."
      (let [y (next-handler x)]
        (if (rejects y)
          (str "I don't like " y)
          (str y))))))

(def increment-string (middleware inc #{2}))
(increment-string 1)

;; Middleware factory
(defn middleware-factory
  [rejects]
  {:pre [(set? rejects)]}
  (fn [handler]
    (fn [x]
      (if (= x 1)
        "I'm not going to do anything..."
        (let [y (handler x)]
          (if (rejects y)
            (str "I don't like " y " : '(")
            (str y)))))))
(def increment-string
  ((middleware-factory #{2}) inc))

(increment-string 1)
