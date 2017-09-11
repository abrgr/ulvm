(ns ^{:author "Adam Berger"} ulvm.runners.http
  "ULVM builtin Docker container runner"
  (:require [ulvm.core :as ucore]
            [ulvm.reader :as r]
            [ulvm.project :as uprj]
            [ulvm.utils :as util]
            [org.httpkit.client :as http]
            [cats.core :as m]
            [cats.monad.either :as e])
  (:import  [java.net.ConnectException]))

(defn- read-response
  [body]
  (-> body
      (java.io.InputStreamReader.)
      (java.io.PushbackReader.)
      (r/edn-seq)))

(defn- retryable-error?
  [val]
  (and (e/left? val)
       (->> (m/extract val)
            (instance? java.net.ConnectException))))

(defmethod uprj/run :ulvm.runners/http
  [prj ctx runner]
  (let [desc                (::ucore/runner-descriptor runner)
        acceptable-statuses (:acceptable-statuses desc)]
    (util/retrying 100
                   5 
                   retryable-error?
                   @(http/request (conj desc {:as :stream})
                                  (fn [{:keys [status body error]}]
                                    (cond
                                      (some? error)                          (e/left error)
                                      (contains? acceptable-statuses status) (e/right (read-response body))
                                      :else                                  (e/left (str "Status code: " status))))))))
