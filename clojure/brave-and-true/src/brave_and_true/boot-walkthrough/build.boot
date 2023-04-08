(deftask specify-this
  "Specify something"
  [s something SOMETHING str "An object"
   p pluralize bool "Use 'are' or 'is'"]
  (fn middleware [next-handler]
    (fn handler [fileset]
      (next-handler
       (merge fileset {:something something
                       :pluralize pluralize})))))


(deftask print-this
  "Prints something on the screen..."
  []
  (fn middleware [next-handler]
    (fn handle [fileset]
      (let [verb (if (:pluralize fileset) "are" "is")]
        (println (:something fileset) verb "on the screen")
        fileset))))


