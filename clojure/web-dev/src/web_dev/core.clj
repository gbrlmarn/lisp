(ns web-dev.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            #_[aleph.http :as aleph]
            [compojure.core :as comp]
            [compojure.route :as route]
            [clojure.pprint :as pprint]))

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

;; Server object is saved into an atom
(defonce server (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application routes
(defn app [req] ;; Handler app
  (case (:uri req)
    "/"
    {:status 200
     :body "Homepage"
     :headers {"Content-type" "text/html; charset=UTF-8"}}
    "/echo"
    {:status 200
     :body (with-out-str (pprint/pprint req))
     :headers {"Content-type" "text/plain"}}
    "/greeting"
    {:status 200
     :body "Hello, Mister"
     :header {"Content-type" "text/plain"}}
    {:status 404
     :body "Not found."
     :headers {"Content-type" "text/plain"}}))

;; Application routes with Compojure
(comp/defroutes routes
  (comp/GET "/" []
    {:status 200
     :body "<h1>Homepage</h1>
<ul>
<li><a href=\"/echo\">Echo request</a></li>
<li><a href=\"greeting\">Greeting</a></li>
</ul>"
     :header {"Content-Type" "text/html; charset=UTF-8"}})
  (comp/ANY "/echo" req
    {:status 200
     :body (with-out-str (pprint/pprint req))
     :headers {"Content-Type" "text/plain"}})
  (comp/GET "/greeting" []
            {:status 200
             :body "Hello Mister"
             :header {"Content-type" "text/plain"}})
  (route/not-found {:status 404
                    :body "Not found."
                    :header {"Content-Type" "text/plain"}}))

(def app routes)

;; Start the server and save it in the atom
(defn start-server []
  (reset! server
          #_(aleph/start-server
             (fn [req] (app req))
             {:port 3001})
          assoc
          (jetty/run-jetty
           (fn [req] (app req))
           {:port 3001
            :join? false})))

;; Stop the server
(defn stop-server []
  (when-some [s @server] ;; chech if there is an object server
    ;;(.close s) ;; aleph stop method
    (.stop s) ;; call the stop method 
    (reset! server nil))) ;; overwrite the atom with nil



;;;;;;;;;;;;;;;;;;;;
;; Function calls ;;
;;;;;;;;;;;;;;;;;;;;
(start-server)
(stop-server)
