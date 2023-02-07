(ns example-static-website.core
  (:require [clojure.string :as str]
            [stasis.core :as stasis]
            [markdown.core :as md]
            [hiccup.page :as hiccup]))

;; vars
(def source-dir "resources")

;; functions

(defn key-to-html [s]
  (str/replace s #".md" ".html"))

(defn read-and-convert! [src]
  (let [data (stasis/slurp-directory src #".*\.md$")
        html-paths (map key-to-html (keys data))
        html-content (map md/md-to-html-string
                          (vals data))]
    (zipmap html-paths html-content)))

(defn apply-header-footer [page]
  (hiccup/html5 {:lang "en"}
                [:head
                 [:title "Static website!"]
                 [:meta {:charset "utf-8"}]
                 [:meta {:name "viewport"
                         :content "width=device-width, initial-scale=1.0"}]
                 [:link {:type "text/css"
                         :href "/css/style.css"
                         :rel "stylesheet"}]
                 [:body
                  [:div {:class "header"}
                   [:div {:class "name"}
                    [:a {:href "/"} "Home page"]
                    [:div {:class "header-right"}
                     [:a {:href "/posts"} "Posts"]]]]
                  page]
                 [:footer
                  [:p "This is the footer"]]]))

(defn get-css [src]
  (stasis/slurp-directory src #".*\.css$"))

(defn format-pages [m]
  (let [html-keys (keys m)
        page-data (map apply-header-footer
                       (vals m))]
    (zipmap html-keys page-data)))

(defn merge-website-assets! [root-dir]
  (let [page-map (format-pages (read-and-convert! root-dir))
        css-map (get-css source-dir)]
    (stasis/merge-page-sources {:css css-map
                                :pages page-map})))
;; end of functions

;; tests

(md/md-to-html-string "# This should be h1")
(md/md-to-html-string "## This should be h2")
(stasis/slurp-directory source-dir #".*\.md$")
(keys (stasis/slurp-directory source-dir
                              #".*\.md$"))
(format-pages (read-and-convert! source-dir))
(apply-header-footer "here is some content!")
(read-and-convert! source-dir)
(str/replace "index.md" #".md" ".html")

;; end of tests

(def server
  (stasis/serve-pages
   (merge-website-assets! source-dir)))

(def out-dir "docs")

(defn export! []
  (stasis/empty-directory! out-dir)
  (stasis/export-pages
   (merge-website-assets! source-dir) out-dir)
  (println "Website is ready!"))
