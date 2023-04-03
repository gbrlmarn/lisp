(ns brave-and-true.chr12
  (:import [java.util Date Stack]
           [java.net Proxy URI]))

;; Java Interop
(macroexpand-1
 '(.toUpperCase "Hmmm"))

;; String java object
(String. "To Davey Jone's Locker with ye hardies")

;; Stack
(let [stack (java.util.Stack.)]
  (.push stack "Latest episode of Game of Thrones")
  (first stack))

;; Multiple methods to object
(doto (java.util.Stack.)
  (.push "Latest episode of Game of Thrones")
  (.push "Not worth it"))

(macroexpand-1
 '(doto (java.util.Stack.)
  (.push "Latest episode of Game of Thrones")
  (.push "Not worth it")))

;; Importing java classes
(Date.)

;; The System Class
