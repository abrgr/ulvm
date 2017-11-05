(ns ^{:author "Adam Berger"} ulvm.mod-combinators.js.sync
  "Synchronous Javascript Module Combinator"
  (:require [ulvm.mod-combinators.js.sync.ast :as ast]
            [org.httpkit.server :as h]
            [compojure.core :as c]
            [ring.middleware.edn :as redn]))

(def default-headers
  {"Content-Type" "application/edn"})

(c/defroutes routes
  (c/POST "/cfg" {:keys [params]}
    {:status  200
     :headers default-headers
     :body    (pr-str
                (assoc-in
                  params
                  [:attrs :ulvm.core/result-in-invocation-block]
                  true))})
  
  (c/POST "/block" {:keys [params]}
    (let [res (ast/gen params)]
      (if (contains? res :err)
        {:status  400
         :headers default-headers
         :body    (get res :err)}
        {:status  200
         :headers default-headers
         :body    (get res :ast)})))

(defn -main
  [& _]
  (let [app (-> routes
                redn/wrap-edn-params)]
    (h/run-server app {:port 8080})))
