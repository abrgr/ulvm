(ns ^{:author "Adam Berger"} ulvm.mod-loaders
  "ULVM loader definition and builtin loaders"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.re-loaders :as rel]
            [ulvm.spec-utils :as su]
            [cats.core :as m]
            [cats.monad.either :as e]))

; TODO: actually implement this
(deftype CustomModLoader [runnable-env]
  uprj/ModLoader
  (-load-module [this prj module-descriptor] (e/right {})))

(defn- get-rel-name
  "Runnable env loader name from loader entity"
  [loader-entity]
  (let [rel (::ucore/runnable-env-ref loader-entity)]
    (or (::ucore/builtin-runnable-env-loader-name rel)
        (::ucore/runnable-env-loader-name rel))))

(defmethod uprj/make-mod-loader :default
  [proj loader-name loader-entity]
  (let [{:keys [:prj :runnable-env]} (uprj/deref-runnable-env proj loader-entity)]
    (uprj/set
     prj
     :mod-loaders
     loader-name
     (e/right (CustomModLoader. runnable-env)))))

; Load our builtin loader implementations
(load "mod_loaders/builtin_docker_hub_mod_loader")
