(ns brave-and-true.chr8)

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
