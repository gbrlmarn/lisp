;; Inspired from "https://www.youtube.com/watch?v=KKvancXJJJg"
;; Fetching information about films and dispalying it in html using hiccup formating

(ns core
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [hiccup.core :as hiccup]))

(def fetch
  (memoize (fn [url]
             (-> (http/get url)
                 :body
                 (json/parse-string keyword)))))

(def title->info
 (into {} (map (juxt :title identity))
       (fetch 
        "https://ghibliapi.vercel.app/films")))

(defn fetch-film [title]
  (get title->info title))

#_(fetch-film "Princess Mononoke")


(->>
 (fetch-film "Princess Mononoke")
 :people
 (map fetch)
 (map :name)
 sort)

(defn ->hiccup [info]
  [:li
   [:h1 (:title info)]
   [:p (:description info)]
   [:img {:src (:image info)}]])

(spit "ghibli.html"
      (hiccup/html
          [:html
           (into [:ul]
                 (map ->hiccup (fetch "https://ghibliapi.vercel.app/films")))]))

(clojure.java.browse/browse-url "ghibli.html")


