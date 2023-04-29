(ns blog-example.web
  (:require [stasis.core :as stasis]
            [hiccup.page :refer [html5]]
            [clojure.java.io :as io]))

;; Default pages layout
(defn layout-page [page]
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1.0"}]
    [:title "Gabriel Marin"]
    [:link {:rel "stylesheet" :href "/styles/styles.css"}]]
   [:body
    [:div.logo "gbrlmarn"]
    [:div.body page]]))

;; About page path
(defn about-page [request]
  (layout-page (slurp (io/resource "partials/about.html"))))

;; Function for serving all partials pages
(defn partial-pages [pages]
  (zipmap (keys pages)
          (map layout-page (vals pages))))

;; Get pages from folders
(defn get-pages []
  (stasis/merge-page-sources
   {:public (stasis/slurp-directory
             "resources/public" #".*.(html|css|js)$")
    :partials (partial-pages
               (stasis/slurp-directory
                "resources/partials" #".*.html$"))}))

;; Serving the pages to web server 
(def app (stasis/serve-pages get-pages))
