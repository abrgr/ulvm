(ns ^{:author "Adam Berger"} ulvm.runners.http
  "ULVM builtin Docker container runner"
  (:require [ulvm.core :as ucore]
            [ulvm.reader :as r]
            [ulvm.project :as uprj]
            [org.httpkit.client :as http]
            [cats.monad.either :as e]))

(defn- read-response
  [body]
  (-> body
      (java.io.InputStreamReader.)
      (java.io.PushbackReader.)
      (r/edn-seq)))

(defmethod uprj/run :ulvm.runners/http
  [prj ctx runner]
  (let [desc                (::ucore/runner-descriptor runner)
        acceptable-statuses (:acceptable-statuses desc)]
    (http/request (conj desc {:as :stream})
                  (fn [{:keys [status body error]}]
                    (cond
                      (some? error)                          (e/left error)
                      (contains? acceptable-statuses status) (e/right (read-response body))
                      :else                                  (e/left (str "Status code: " status)))))))
