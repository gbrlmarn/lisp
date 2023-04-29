(defproject blog-example "0.1.0-SNAPSHOT"
  :description "Gabriel Marin Blog"
  :url "http://gbrlmarn.com"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [stasis "2.5.1"]
                 [ring "1.10.0"]
                 [hiccup "1.0.5"]
                 [markdown-clj "1.11.4"]]
  :ring {:handler blog-example.web/app}
  :profiles {:dev {:plugins [[lein-ring "0.12.6"]]}}
  :repl-options {:init-ns blog-example.core})
