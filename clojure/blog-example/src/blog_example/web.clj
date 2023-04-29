(ns blog-example.web
  (:require [stasis.core :as stasis]
            [hiccup.page :refer [html5]]
            [clojure.string :as str]
            [markdown.core :as markdown]))

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

;; Function for serving all partials pages
(defn partial-pages [pages]
  (zipmap (keys pages)
          (map layout-page (vals pages))))

;; Function for serving all partials pages
(defn markdown-pages [pages]
  (zipmap (map #(str/replace % #"\.md$" "/")
               (keys pages))
          (map layout-page
               (map markdown/md-to-html-string
                    (vals pages)))))

;; Get pages from folders
(defn get-pages []
  (stasis/merge-page-sources
   {:public
    (stasis/slurp-directory
     "resources/public" #".*.(html|css|js)$")
    :partials
    (partial-pages
     (stasis/slurp-directory
      "resources/partials" #".*.html$"))
    :markdown
    (markdown-pages
     (stasis/slurp-directory
      "resources/md" #".md$"))}))

;; Serving the pages to web server 
(def app (stasis/serve-pages get-pages))
