(ns ^{:author "Adam Berger"} ulvm.re-loaders
  "ULVM runnable env loaders definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.runnable-envs :as re]
            [ulvm.project :as uprj]
            [cats.core :as m]
            [cats.monad.either :as e]))

; TODO: actually implement this
(deftype CustomREnvLoader [renv]
  uprj/REnvLoader
  (-get-runnable-env-rep [this prj desc] (e/right {})))

(defmethod uprj/make-renv-loader :default
  [proj re-loader-name re-loader-entity]
  (println re-loader-name)
  (let [{prj :prj, runnable-env :el} (uprj/deref-runnable-env proj re-loader-entity)]
    (uprj/set
     prj
     :renv-loaders
     re-loader-name
     (e/right (CustomREnvLoader. runnable-env)))))

(load "re_loaders/builtin_project_file_re_loader")
