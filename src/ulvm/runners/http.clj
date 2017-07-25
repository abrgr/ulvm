(ns ^{:author "Adam Berger"} ulvm.runners.http
  "ULVM builtin Docker container runner"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [org.httpkit.client :as http]
            [cats.monad.either :as e]))

(defmethod uprj/run :ulvm.runners/http
  [prj ctx runner]
  (let [desc                (::ucore/runner-descriptor runner)
        acceptable-statuses (:acceptable-status-codes desc)]
    (http/request desc
                  (fn [{:keys [status body error]}]
                    (cond
                      (some? error)                          (e/left error)
                      (contains? acceptable-statuses status) (e/right body)
                      :else                                  (e/left (str "Status code: " status)))))))
