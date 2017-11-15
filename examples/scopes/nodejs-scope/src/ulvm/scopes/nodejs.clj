(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs
  "Synchronous Javascript Module Combinator"
  (:require [ulvm.scopes.nodejs.write-deps :as write-deps]
            [org.httpkit.server :as h]
            [compojure.core :as c]
            [ring.middleware.edn :as redn]))

(def default-headers
  {"Content-Type" "application/edn"})

(c/defroutes routes
  (c/POST "/cfg" {:keys [params]}
    (println "Processing /cfg :" params)
    {:status  200
     :headers default-headers
     :body    (pr-str params)})

  (c/POST "/mod-cfg" {:keys [params]}
    (println "Processing /mod-cfg :" params)
    {:status  200
     :headers default-headers
     :body    (pr-str params)})

  (c/POST "/implicit-mods" {:keys [params]}
    (println "Processing /implicit-mods :" params)
    {:status  200
     :headers default-headers
     :body    (pr-str {})})

  (c/POST "/resolve-name" {:keys [params]}
    (println "Processing /resolve-name :" params)
    {:status  200
     :headers default-headers
     :body    (pr-str 'todo)})
  
  (c/POST "/write-deps" {:keys [params]}
    (println "Processing /write-deps :" params)
    (let [{:keys [cfg mod-descs]} params
           res                    (write-deps/write cfg mod-descs)]
      (if (contains? res :err)
        {:status  400
         :headers default-headers
         :body    (get res :err)}
        (do (println "RETURNING: " res) {:status  200
         :headers default-headers
         :body    (get res :res)})))))

(defn -main
  [& _]
  (let [app (-> routes
                redn/wrap-edn-params)]
    (h/run-server app {:port 8080})))
