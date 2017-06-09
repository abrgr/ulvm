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
         ::ucore/runnable-env-loader
         ::ucore/runnable-env
         ::ucore/runner]))

(defprotocol REnvLoader
  (-get-runnable-env-rep [renv-loader prj desc]
    "Retrieves a rep for a runnable environment given the descriptor pointing to it"))

(defn get-runnable-env-rep
  "Retrieves a representation of a runnable environment given a descriptor pointing to it"
  [renv-loader prj desc]
  (-get-runnable-env-rep renv-loader prj desc))

(s/fdef get-runnable-env-rep
        :args (s/cat :renv-loader #(satisfies? REnvLoader %)
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

(s/def ::project
  (s/keys
   :req-un [::env
            ::entities
            ::mod-loaders
            ::renv-loaders
            ::renvs]))

(s/def ::prj ::project)

(s/def ::el (or #(satisfies? ModLoader %)
                #(satisfies? REnvLoader %)))

(defn get-prj-el
  "Get or create a project element"
  [prj n maker prj-el ent-key]
  (let [orig (get-in prj [prj-el n])]
    (cond
      (some? orig) {:prj prj, :el orig}
      :else (let [ent (get-in prj [:entities ent-key n])
                  new-prj (maker prj n ent)]
              {:prj new-prj
               :el (get-in prj [prj-el n])}))))

(s/fdef get-prj-el
        :args (s/cat :prj    ::project
                     :n      keyword?
                     :maker  ifn?
                     :prj-el #{:env
                               :entities
                               :mod-loaders
                               :renv-loaders
                               :renvs}
                     :ent-key keyword?)
        :ret (s/keys :req-un [::prj ::el]))

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

(defn- get-rel-name
  "Runnable env loader name from runnable env ref"
  [re-ref]
  (or (::ucore/builtin-runnable-env-loader-name re-ref)
      (::ucore/runnable-env-loader-name re-ref)))

(defn deref-runnable-env
  "Retrieve a runnable env from an entity that contains a runnable-env-ref."
  [proj entity]
  (let [re-ref (::ucore/runnable-env-ref entity)
        re-desc (::ucore/runnable-env-descriptor re-ref)
        rel-name (get-rel-name re-ref)
        {prj :prj, renv-loader :el} (get proj ::ucore/runnable-env-loader rel-name)
        renv-rep (get-runnable-env-rep renv-loader prj re-desc)]
    {:prj prj, :runnable-env renv-rep}))

(s/fdef deref-runnable-env
        :args (s/cat :proj ::project
                     :entity (s/keys :req [::ucore/runnable-env-ref]))
        :ret (s/keys :req-un [::prj
                              ::ucore/runnable-env]))

