(defproject example-static-website "0.1.0-SNAPSHOT"
  :description "Code that generates marinmgabriel.github.io"
  :url "https://marinmgabriel.github.io"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [stasis "2.5.0"]
                 [markdown-clj "1.10.5"]
                 [hiccup "1.0.5"]
                 [ring "1.8.2"]
                 [optimus "2023.11.21"]]
  :ring {:handler example-static-website.core/server}
  :profiles {:dev {:plugins [[lein-ring "0.12.5"]]}}
  :repl-options {:init-ns example-static-website.core}
  :aliases {"build-site" ["run" "-m" "example-static-website.core/export!"]})

