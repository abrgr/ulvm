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
     [loaders [loader-name loader-entity]]
     (assoc loaders
            loader-name
            (get-loader loader-name loader-entity env)))
   {}
   loader-entities))

(s/fdef resolve-loaders
        :args (s/cat
               :loader-entities (s/+ :ulvm.core/loader)
               :env map?)
        :ret (s/map-of keyword? :l/any-loader))

(defn- get-loader
  "Creates or retrieves the loader for a loader entity, using the provided environment"
  [loader-name loader-entity env]
  (if (contains? @loaders loader-name)
    (get @loaders loader-name)
    ; TODO: lock during creation
    (let [loader (make-loader loader-name loader-entity env)]
      (swap! loaders assoc loader-name loader)
      loader)))

(s/fdef get-loader
        :args (s/cat :loader-name keyword?
                     :loader-entity :ucore/loader
                     :env map?)
        :ret ::any-loader)

(defmulti make-loader
  "Creates the loader for a loader entity"
  (fn make-loader-dispatcher
    [loader-name loader-entity env] loader-name))

(s/fdef make-loader
        :args (s/cat :loader-name string?
                     :loader-entity :ucore/loader
                     :env map?)
        :ret ::any-loader)

(defmethod make-loader :default
  [loader-name loader-entity env]
  nil) ; TODO: implement non-builtin loaders

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