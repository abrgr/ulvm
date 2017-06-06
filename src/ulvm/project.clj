(ns ^{:author "Adam Berger"} ulvm.project
  "Project definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [cats.core :as m])
  (:refer-clojure :exclude [get set]))

(s/def ::entities
  (s/keys
   :opt [::ucore/flow
         ::ucore/scope
         ::ucore/mod-loader
         ::ucore/runnable-env
         ::ucore/runner]))

(defprotocol REnvLoader
  (-get-runnable-env-rep [this prj desc]
    "Retrieves a rep for a runnable environment given the descriptor pointing to it"))

(defn get-runnable-env-rep
  "Retrieves a representation of a runnable environment given a descriptor pointing to it"
  [this prj desc]
  (-get-runnable-env-rep this prj desc))

(s/fdef get-runnable-env-rep
        :args (s/cat :this #(satisfies? REnvLoader %)
                     :prj ::project
                     :desc :map?)
        :ret (su/either-of? su/any ::ucore/runnable-env))

(defprotocol ModLoader
  (-load-module [this prj module-descriptor] "Loads a module, returning an either[error, env]"))

(defn load-module
  "Loads a module"
  [this prj module-descriptor]
  (-load-module this prj module-descriptor))

(s/fdef load-module
        :args (s/cat :loader #(satisfies? ModLoader %)
                     :prj ::project
                     :module-descriptor ::ucore/module-descriptor)
        :ret (su/either-of? su/any map?))

(s/def ::mod-loaders
  (s/map-of
   keyword?
   (su/either-of? su/any
                  #(satisfies? ModLoader %))))

(s/def ::renv-loaders
  (s/map-of
   keyword?
   (su/either-of? su/any
                  #(satisfies? REnvLoader %))))

; TODO: define renvs

(s/def ::project
  (s/keys
   :req-un [::env
            ::entities
            ::mod-loaders
            ::renv-loaders
            ::renvs]))

(defn get-prj-el
  "Get or create a project element"
  [prj n maker prj-el ret-key ent-key]
  (let [orig (get-in prj [prj-el n])]
    (cond
      (some? orig) {:prj prj, ret-key orig}
      :else (let [ent (get-in prj [:entities ent-key n])
                  new-prj (maker prj n ent)]
              {:prj new-prj
               ret-key (get-in prj [prj-el n])}))))

(s/fdef get-prj-el
        :args (s/cat :prj    ::project
                     :n      keyword?
                     :maker  ifn?
                     :prj-el #{:env
                               :entities
                               :mod-loaders
                               :renv-loaders
                               :renvs}
                     :ret-key keyword?
                     :ent-key keyword?)
        :ret ::project)

(defn set-prj-el
  "Set a project element"
  [prj type name item]
  (assoc-in prj [type name] item))

(defmulti get
  "Get the project sub-item of specified type"
  (fn get-dispatcher [prj type name] type))

(defmulti set
  "Set the project sub-item of specified type"
  (fn set-dispatcher [prj type name item] type))

(defn get-env
  "Get an environment value"
  [prj key]
  (get-in prj [:env key]))
