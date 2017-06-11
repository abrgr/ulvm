(ns ^{:author "Adam Berger"} ulvm.project
  "Project definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [cats.monad.either :as e]
            [cats.core :as m])
  (:refer-clojure :exclude [get set]))

(declare get-runnable-env-rep
         load-module
         get-prj-ent
         get-or-make
         get
         set
         get-env
         make-renv
         deref-runnable-env)

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
                     :desc map?)
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

(s/def ::env map?)

(s/def ::project
  (s/keys
   :req-un [::env
            ::entities
            ::mod-loaders
            ::renv-loaders
            ::renvs]))

(s/def ::prj ::project)

(s/def ::el (su/either-of?
             su/any
             (or #(satisfies? ModLoader %)
                 #(satisfies? REnvLoader %))))
                 

(defmulti make-mod-loader
  "Make an instance of a module loader, given the corresponding entity"
  (fn make-mod-loader-dispatcher [prj name entity] name))

(s/fdef make-mod-loader
        :args (s/cat :prj ::project
                     :loader-name keyword?
                     :loader-entity (s/nilable ::ucore/mod-loader))
        :ret ::project)

(defmulti make-renv-loader
  "Make an instance of a runnable env loader, given the corresponding entity"
  (fn make-renv-loader-dispatcher [prj name entity] name))

(s/fdef make-renv-loader
        :args (s/cat :prj ::project
                     :loader-name keyword?
                     :loader-entity (s/nilable ::ucore/runnable-env-loader))
        :ret ::project)

(defn set
  "Set a project element"
  [prj type name item]
  (assoc-in prj [type name] item))

(defn- entity-type-from-prj-type
  [prj-type]
  (prj-type {:mod-loaders ::ucore/mod-loaders
             :renv-loaders ::ucore/runnable-env-loaders}))

(defn- maker-from-prj-type
  [prj-type]
  (prj-type {:mod-loaders  make-mod-loader
             :renv-loaders make-renv-loader
             :renvs        make-renv}))

(defn get-prj-ent
  [prj ent-key name]
  (get-in prj [:entities ent-key name]))

(defn get-or-make
  "Get or make a project element"
  [prj type id entity]
  (let [orig (get-in prj [type id])]
    (if (some? orig)
      {:prj prj, :el orig}
      (let [make (maker-from-prj-type type)
            new-prj (make prj id entity)]
        {:prj new-prj
         :el (get-in new-prj [type id])}))))

(s/fdef get-or-make
        :args (s/cat :prj  ::project
                     :type #{:env
                             :entities
                             :mod-loaders
                             :renv-loaders
                             :renvs}
                     :id su/any
                     :entity su/any)
        :ret (s/keys :req-un [::prj ::el]))

(defn get
  "Get or create a project element"
  [prj type id]
  (let [ent-key (entity-type-from-prj-type type)]
    (get-or-make prj type id (get-prj-ent prj ent-key id))))

(s/fdef get
        :args (s/cat :prj  ::project
                     :type #{:env
                             :entities
                             :mod-loaders
                             :renv-loaders
                             :renvs}
                     :name keyword?)
        :ret (s/keys :req-un [::prj ::el]))

(defn get-env
  "Get an environment value"
  [prj key]
  (get-in prj [:env key]))

(defn- get-rel-name
  "Runnable env loader name from runnable env ref"
  [re-ref]
  (or (::ucore/builtin-runnable-env-loader-name re-ref)
      (::ucore/runnable-env-loader-name re-ref)))

(defn- make-renv
  "Make a runnable-env (get a runnable env ref)"
  [proj renv-id entity]
  (let [re-ref (::ucore/runnable-env-ref entity)
        re-desc (::ucore/runnable-env-descriptor re-ref)
        rel-name (get-rel-name re-ref)
        {prj :prj, rel-either :el} (get proj :renv-loaders rel-name)]
    (set
     prj
     :renvs
     renv-id
     (m/mlet [renv-loader rel-either]
       (get-runnable-env-rep renv-loader prj re-desc)))))

(s/fdef make-renv
 :args (s/cat :proj    ::project
              :renv-id ::ucore/runnable-env-ref
              :entity  (s/keys :req [::ucore/runnable-env-ref]))
 :ret (s/keys :req-un [::prj ::el]))

(defn deref-runnable-env
  "Retrieve a runnable env from an entity that contains a runnable-env-ref."
  [prj entity]
  (let [re-ref (::ucore/runnable-env-ref entity)]
    (get-or-make prj :renvs re-ref entity)))

(s/fdef deref-runnable-env
        :args (s/cat :proj ::project
                     :entity (s/keys :req [::ucore/runnable-env-ref]))
        :ret (s/keys :req-un [::prj
                              ::el]))
