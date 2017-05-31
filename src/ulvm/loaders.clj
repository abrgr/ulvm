(ns ulvm.loaders
  "ULVM loader definition and builtin loaders"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]))

(declare resolve-loaders
         get-loader
         make-loader
         launch
         load-module
         stop)

(def loaders (atom {}))

(defprotocol Loader
  (-launch [this] "Launches this loader")
  (-load-module [this module-descriptor] "Loads a module")
  (-stop [this] "Stops a loader"))

(s/def ::load-result
  (s/keys
   :req [(or ::err ::success)]))

(s/def ::any-loader #(extends? Loader %))

(defn resolve-loaders
  "Resolves a seq of loader entities into actual loaders"
  [loader-entities env]
  (reduce
   (fn resolve-loader-reducer
     [loaders loader-name]
     (assoc loaders
            loader-name
            (get-loader loader-name loader-entities env)))
   {}
   (keys loader-entities)))

(s/fdef resolve-loaders
        :args (s/cat
               :loader-entities (s/map-of keyword? :ulvm.core/loader)
               :env map?)
        :ret (s/map-of keyword? :l/any-loader))

; TODO: this should return an either
(defn- get-loader
  "Creates and launches or retrieves the loader for a loader entity using the provided environment"
  [loader-name loader-entities env]
  (if (contains? @loaders loader-name)
    (get @loaders loader-name)
    ; TODO: lock during creation
    (let [loader (make-loader loader-name loader-entities env)
          launch-res (launch loader)]
      (if (::success launch-res)
        (do
          (swap! loaders assoc loader-name loader)
          loader)
        launch-res))))

(s/fdef get-loader
        :args (s/cat :loader-name keyword?
                     :loader-entities (s/map-of keyword? :ulvm.core/loader)
                     :env map?)
        :ret (or ::load-result ::any-loader))

(defmulti make-loader
  "Creates the loader for a loader entity"
  (fn make-loader-dispatcher
    [loader-name loader-entities env] loader-name))

(s/fdef make-loader
        :args (s/cat :loader-name string?
                     :loader-entities (s/map-of keyword? :ulvm.core/loader)
                     :env map?)
        :ret ::any-loader)

(defn- get-parent-loader
  [loader-entity loader-entities env]
  (let [module (:ucore/module loader-entity)
        builtin-parent-name (:ucore/builtin-loader-name module)
        custom-parent-name (:ucore/loader-name module)
        parent-loader-name (or builtin-parent-name custom-parent-name)]
    (get-loader parent-loader-name loader-entities env)))

; TODO: actually implement this
(deftype CustomLoader [load-result env]
  Loader
  (-launch [this] {::success true})
  (-load-module [this module-descriptor] {::success true})
  (-stop [this] {::success true}))

(defmethod make-loader :default
  [loader-name loader-entities env]
  (let [parent-loader (get-parent-loader (loader-name loader-entities) loader-entities env)
        mod-desc (:ucore/module-descriptor module)
        load-res (load-module parent-loader mod-desc)]
    (CustomLoader. load-res env)))

(defn launch
  "Launches a loader"
  [this]
  (-launch this))

(s/fdef launch
        :args (s/cat :loader ::any-loader)
        :ret ::load-result)

(defn load-module
  "Loads a module"
  [this module-descriptor]
  (-load-module this module-descriptor))

(s/fdef launch
        :args (s/cat :loader ::any-loader
                     :module-descriptor :ulvm.core/module-descriptor)
        :ret ::load-result)

(defn stop
  "Stops a loader"
  [this]
  (-stop this))

(s/fdef stop
        :args (s/cat :loader ::any-loader)
        :ret ::load-result)

; Load our builtin loader implementations
(load "loaders/docker-hub-loader")
