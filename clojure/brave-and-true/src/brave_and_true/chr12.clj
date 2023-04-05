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
(System/getenv)
(System/getProperty "user.dir")
(System/getProperty "java.version")

;; The Date Class
(java.util.Date.)
(.toString (java.util.Date.))


;; Input/Output
(let [file (java.io.File. "/")]
  (println (.exists file))
  (println (.canWrite file))
  (println (.getPath file)))

(spit "/tmp/groceries"
      "- Pasta
- Tomatoes")
(slurp "/tmp/groceries")

(let [s (java.io.StringWriter.)]
  (spit s "capture this")
  (.toString s))

(let [s (java.io.StringReader.
         "- something that is captured")]
  (slurp s))

(with-open [todo-list
            (clojure.java.io/reader
             "/tmp/groceries")]
  (first (line-seq todo-list)))


