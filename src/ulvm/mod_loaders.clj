(ns ^{:author "Adam Berger"} ulvm.mod-loaders
  "ULVM loader definition and builtin loaders"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.re-loaders :as rel]
            [ulvm.spec-utils :as su]
            [cats.core :as m]
            [cats.monad.either :as e]))

(defmulti make
  "Creates the loader for a loader entity, adding it to the project"
  (fn make-dispatcher
    [prj loader-name loader-entity] loader-name))

(s/fdef make
        :args (s/cat :prj ::uprj/project
                     :loader-name keyword?
                     :loader-entity ::ucore/mod-loader)
        :ret ::uprj/project)

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

(defmethod uprj/get ::ucore/mod-loader
  [prj _ name]
  (uprj/get-prj-el prj name make :mod-loaders :mod-loader ::ucore/mod-loaders))

(defmethod uprj/set ::ucore/mod-loader
  [prj type name item]
  (uprj/set-prj-el prj type name item))

(defmethod make :default
  [proj loader-name loader-entity]
  (let [rel-name (get-rel-name loader-entity)
        {:keys [:prj :renv-loader]} (uprj/get proj ::ucore/runnable-env-ref rel-name)
        re nil]
    ; TODO: actually load the environment
    (uprj/set prj ::ucore/mod-loader loader-name (CustomModLoader. re))))

; Load our builtin loader implementations
(load "mod_loaders/builtin_docker_hub_mod_loader")
