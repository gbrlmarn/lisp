(ns web-dev.core
  (:require [ring.adapter.jetty :as jetty]))

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

;; Server object is saved into an atom
(defonce server (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data to display
(defn app [req] ;; Handler app
  {:status 200
   :body "Hello World Dynamic"
   :header {}})

;; Start the server and save it in the atom
(defn start-server []
  (reset! server
          (jetty/run-jetty
           (fn [req] (app req))
           {:port 3001
            :join? false})))

;; Stop the server
(defn stop-server []
  (when-some [s @server] ;; chech if there is an object server
    (.stop s) ;; call the stop method 
    (reset! server nil))) ;; overwrite the atom with nil



;;;;;;;;;;;;;;;;;;;;
;; Function calls ;;
;;;;;;;;;;;;;;;;;;;;
(start-server)
(stop-server)
