(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs
  "NodeJS Scope"
  (:require [ulvm.scopes.nodejs.write-deps :as write-deps]
            [ulvm.scopes.nodejs.resolve-name :as resolve-name]
            [ulvm.scopes.nodejs.write-flow :as write-flow]
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
    (let [{:keys [name-parts]} params]
      {:status  200
       :headers default-headers
       :body    (pr-str {:name (resolve-name/n name-parts)})}))

  (c/POST "/write-flow" {:keys [params]}
    (println "Processing /write-flow :" params)
    (let [{:keys [cfg
                  flow-name
                  flow-args
                  flow-ast]} params
          res                (write-flow/f cfg flow-name flow-args flow-ast)]
      (if (contains? res :err)
        {:status  400
         :headers default-headers
         :body    (pr-str (get res :err))}
        {:status  200
         :headers default-headers
         :body    (pr-str (get res :res))})))
  
  (c/POST "/write-deps" {:keys [params]}
    (println "Processing /write-deps :" params)
    (let [{:keys [cfg mod-descriptors]} params
           res                    (write-deps/write cfg mod-descriptors)]
      (if (contains? res :err)
        {:status  400
         :headers default-headers
         :body    (get res :err)}
        (do (println "RETURNING: " res)
            {:status  200
             :headers default-headers
             :body    (pr-str (get res :res))})))))

(defn -main
  [& _]
  (let [app (-> routes
                redn/wrap-edn-params)]
    (h/run-server app {:port 8080})))
