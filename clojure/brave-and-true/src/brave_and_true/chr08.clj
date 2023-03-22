(ns brave-and-true.chr08)

;; Writing macros
(macroexpand '(when boolean-expression
                expression-1
                expression-2
                expression-3))

(defmacro infix
  "Use this macro when you pine for the notation of your childhood"
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))
(macroexpand '(infix (1 + 3)))
(infix (1 + 2))

(defmacro infix-2
  [[operand1 op operand2]]
  (list op operand1 operand2))
(infix-2 (1 + 2))
(macroexpand '(infix-2 (1 + 2)))

(defmacro my-print-whoopsie
  [expression]
  (list 'let ['result expression]
        (list println 'result)
        'result))
(my-print-whoopsie "madame")
(macroexpand '(my-print-whoopsie "mister"))

(macroexpand '(when (the-cows-come :home)
                (call me :pappy)
                (slap me :betty)))

(defmacro unless
  "Inverted 'if'"
  [test & branches]
  (conj (reverse branches) test 'if))

(macroexpand '(unless (is-done-right)
                      (tell-me "Omg men...")
                      (slap me :betty)))
;; 'quote' vs 'syntax quote'
'+
`+

'(+ 1 2)
`(+ 1 2)

;; syntax quoting includes namespaces to avoid colisions
;; syntax quoting includes unquoting '~'
`(+ 1 ~(inc 1))

(list '+ 1 (inc 1))
`(+ 1 ~(inc 1))

;; code-critic 'list version'
(defmacro code-critic
  "Phrases are coutesy Hermes Conrad from Futurama"
  [bad good]
  (list 'println
        "Great squid of Madrid, this is bad code:"
        (list 'quote bad))
  (list 'println
        "Sweet gorilla of Manila, this is good code:"
        (list 'quote good)))
(code-critic (1 + 1) (+ 1 1))

;; code-critic 'syntax quoting version'
(defmacro code-critic
  "Phrases are coutesy Hermes Conrad from Futurama"
  [bad good]
  `(println "Great squit of Madrid, this is bad code:"
            (quote ~bad))
  `(println "Sweet gorilla of Manila, this is good code:"
            (quote ~good)))
(code-critic (1 + 1) (+ 1 1))

;; Refactoring a Macro and Unquoate Splicing
(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))
(defmacro code-critic
  [bad good]
  `(do ~(criticize-code "Cursed bacteria of Liberia, this is bad code:" bad)
       ~(criticize-code "Sweet sacred boa of Western and Easern Samoa, this is good code" good)))
(code-critic (1 + 1) (+ 1 1))

;; When you call a function multiple times
;; it makes sense to use 'map'.
(defmacro code-critic
  [bad good]
  `(do ~@(map #(apply criticize-code %)
             [["Great squit of Madrid, this is bad code: " bad]
              ["Sweet gorilla of Manila, this is good code: " good]])))
(code-critic (1 + 2) (+ 1 2))

;; unquote '~' vs unquote splice '~@'
`(+ ~(list 1 2 3))
`(+ ~@(list 1 2 3))

;; Variable Capture
(def message "Good job")
(defmacro with-mischief
  [& stuff-to-do]
  (concat (list 'let ['message "Oh, big deal!"])
          stuff-to-do))
(with-mischief
  (println "Here's how I feel about that thing you did: "
           message))
;; The macro creates new binding to message
(defmacro with-mischief
  [& stuff-to-do]
  `(let [message "Oh, big deal!"]
     ~@stuff-to-do))
(with-mischief
  (println "Here's how I feel about that thing you did: "
           message))

;; Gensym produces unique symbols and prevents variable capture
(gensym)
(gensym 'message)

;; without-mischief using gensym
(defmacro without-mischief
  [& stuff-to-do]
  (let [macro-message (gensym 'message)]
    `(let [~macro-message "Oh, big deal!"]
       ~@stuff-to-do
       (println "I still need to say: " ~macro-message))))
(without-mischief
  (println "Here's how I feel about that thing you did: "
           message))

;; Autogensym
`(blarg# bluez#)
`(let [name# "Laryy Potter"] name#)

;; Double evaluation
(defmacro report
  [to-try]
  `(if ~to-try
     (println (quote ~to-try) "was successful:" ~to-try)
     (println (quote ~to-try) "was not successful:" ~to-try)))
(report (+ 1 1))
(macroexpand
 '(report (do (Thread/sle 1000) (+ 1 1))))

;; Put 'to-try' in a let expression and eval only once
(defmacro report
  [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful:" result#)
       (println (quote ~to-try) "was not successful:" result#))))

(report (= 1 1))
(report (= 1 2))
(doseq [code ['(= 1 1) '(= 1 2)]]
  (report code))

(macroexpand
 '(doseq [code ['(= 1 1) '(= 1 2)]]
    (report code)))

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] (list macroname arg)) args)))
(doseq-macro report (= 1 1) (= 1 2))
(macroexpand
 '(doseq-macro report (= 1 1) (= 1 2)))

;; Macros in practice
(def order-details
  {:name "mitcard Blimmons"
   :email "mitchard.blimmonsgmail.com"})
(def order-details-validation
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter a email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})
(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))
(defn validate
  "Retturns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-group] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-group)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))
(validate order-details order-details-validation)

;; 'if-valid' macro
(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))
(macroexpand
 '(if-valid order-details
    order-details-validation
    my-error-name
    (println :success)
    (println :failure my-error-name)))

(seq '1)

;; Exercises
;; 1. Write the macro when-valid so that it behaves similary to when. Here is an example of calling it:
(def order-details-good
  {:name "mitcard Blimmons"
   :email "mitchard.blimmons@gmail.com"})
(def order-details-bad
  {:name "mitcard Blimmons"
   :email "mitchard.blimmonsgmail.com"})

(defmacro when-valid
  [data data-validation & actions]
  `(if-valid ~data ~data-validation ~'err
             (do ~@actions)
             false))

(when-valid order-details-bad order-details-validation
            (println "It's a success!")
            (println :success))
(when-valid order-details-good order-details-validation
            (println "It's a success!")
            (println :success))
(macroexpand
 '(when-valid order-details-bad order-details-validation
            (println "It's a success!")
            (println :success)))

;; 2. You saw that and is implemented as a macro. Implement or as a macro.
(defmacro example-and
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (example-and ~@next) and#))))
(defmacro example-or
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (example-or ~@next)))))
(or 3 0)
(or 2 3)
(example-or 3 0)
(example-or 2 3)

;; 3. In Chapter 5 you created a series of functions (c-int, c-str, c-dex) to read an RPG character's attribute. Write a macro that defines an arbitrary number of attribute-retrieving functions usin one macro call. Here's how you would call it:
(defattrs
  c-int :intelligence
  c-str :strength
  c-dex :dexterity)
(macroexpand
 '(defattrs
  c-int :intelligence
  c-str :strength
  c-dex :dexterity))
(defmacro defattrs
  [& attributes]
  `(do
     ~@(map (fn [[f-name attr-name]]
              `(def ~f-name (comp ~attr-name :attributes)))
            (partition 2 attributes))))
(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(c-int character)
(c-str character)
(c-dex character)
(def c-int (comp :intelligence :attributes))
