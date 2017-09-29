(ns ^{:author "Adam Berger"} ulvm.project
  "Project definition"
  (:require [clojure.spec :as s]
            [clojure.zip :as z]
            [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [ulvm.func-utils :as futil]
            [cats.monad.either :as e]
            [cats.core :as m])
  (:refer-clojure :rename {get core-get
                           set core-set}))

(declare get-runnable-env-rep
         scope-with-results
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
         ::ucore/mod-combinator
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

(defprotocol ModCombinator
  (-scope-with-results
    [this prj invocations deps consumer result]
    "Returns an AST for a scope in which the results of
     the invocations are available and in which consumer
     is included (consumer consumes the results)."))

(defn scope-with-results
  "Loads a module"
  [this prj invocations deps consumer result]
  (-scope-with-results this prj invocations deps consumer result))

(s/def ::result-name keyword?)
(s/def ::arguments
  (s/map-of keyword?
            (s/cat :sub-result-name keyword?
                   :result-name     symbol?)))

(s/def ::invocation
  (s/keys :req-un [::result-name
                   ::ucore/module-combinator
                   ::arguments]))

(s/fdef scope-with-results
        :args (s/cat :combinator  #(satisfies? ModCombinator %)
                     :prj         ::project
                     :invocations (s/+ ::invocation)
                     :deps        (s/map-of symbol? (s/+ symbol?))
                     :consumer    su/any
                     :result      (s/? symbol?))
        :ret (su/either-of? su/any map?))

(s/def ::mod-combinators
  (s/map-of
   keyword?
   (su/either-of? su/any
                  #(satisfies? ModCombinator %))))

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
            ::mod-combinators
            ::renv-loaders
            ::renvs]))

(s/def ::prj ::project)

(s/def ::el (su/either-of?
             su/any
             (or #(satisfies? ModCombinator %)
                 #(satisfies? REnvLoader %))))

(defprotocol ArtifactLoader
  (-get-artifact [artifact-loader prj desc]
    "Retrieves an artifact given the descriptor pointing to it"))

(defn get-artifact
  "Retrieves an artifact given the descriptor pointing to it"
  [artifact-loader prj desc]
  (-get-artifact artifact-loader prj desc))

(s/fdef get-artifact
        :args (s/cat :artifact-loader #(satisfies? ArtifactLoader %)
                     :prj ::project
                     :desc map?)
        :ret ::project)

(defmulti make-mod-combinator
  "Make an instance of a module combinator, given the corresponding entity"
  (fn make-mod-combinator-dispatcher [prj name entity] name))

(s/fdef make-mod-combinator
        :args (s/cat :prj    ::project
                     :name   keyword?
                     :entity (s/nilable ::ucore/mod-combinator))
        :ret ::project)

(defmulti make-renv-loader
  "Make an instance of a runnable env loader, given the corresponding entity"
  (fn make-renv-loader-dispatcher [prj name entity] name))

(s/fdef make-renv-loader
        :args (s/cat :prj ::project
                     :loader-name keyword?
                     :loader-entity (s/nilable ::ucore/runnable-env-loader))
        :ret ::project)

(defmulti run
  "Invoke a runner"
  (fn run-dispatcher
    [prj ctx runner]
    (::ucore/builtin-runner-name runner)))

(defn set
  "Set a project element"
  [prj type name item]
  (assoc-in prj [type name] item))

(defn- entity-type-from-prj-type
  [prj-type]
  (prj-type {:mod-combinators ::ucore/mod-combinators
             :renv-loaders    ::ucore/runnable-env-loaders}))

(defn- maker-from-prj-type
  [prj-type]
  (prj-type {:mod-combinators make-mod-combinator
             :renv-loaders    make-renv-loader
             :renvs           make-renv}))

(defn get-prj-ent
  [prj ent-key name]
  (get-in prj [:entities ent-key name]))

(defn get-or-make
  "Get or make a project element"
  [prj type id entity]
  (let [orig (get-in prj [type id])]
    (if (some? orig)
      {:prj prj, :el orig}
      (let [make    (maker-from-prj-type type)
            new-prj (make prj id entity)]
        {:prj new-prj
         :el (get-in new-prj [type id])}))))

(s/fdef get-or-make
        :args (s/cat :prj  ::project
                     :type #{:env
                             :entities
                             :mod-combinators
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
                             :mod-combinators
                             :renv-loaders
                             :renvs}
                     :name keyword?)
        :ret (s/keys :req-un [::prj ::el]))

(defn get-ctx-env
  "Get an environment value, raising the sub-keys
   of a given path (context) to the top level"
  ([prj ctx key-path]
   (get-ctx-env prj ctx key-path nil))
  ([prj ctx key-path not-found]
   (or (get-env prj (into ctx key-path) not-found)
       (get-env prj key-path not-found))))

(s/fdef get-ctx-env
        :args (s/cat :prj ::project
                     :ctx sequential?
                     :key-path sequential?
                     :not-found (s/? su/any))
        :ret (su/either-of? su/any su/any))

(defn get-env
  "Get an environment value"
  ([prj key-path]
   (get-env prj key-path nil))
  ([prj key-path not-found]
   (futil/get-in-either prj (cons :env key-path) not-found)))

(s/fdef get-env
        :args (s/cat :prj ::project
                     :key-path sequential?
                     :not-found (s/? su/any))
        :ret (su/either-of? su/any su/any))

(defn set-env
  "Set an environment value"
  [prj key-path value]
  (assoc-in prj (cons :env key-path) value))

(s/fdef set-env
        :args (s/cat :prj ::project
                     :key-path sequential?
                     :value su/any)
        :ret ::project)

(defn- any-zip
  "Zipper for arbitrary builtin data structures"
  [root]
  (z/zipper
   coll?
   (fn [x]
     (cond
       (map? x) (seq x)
       (set? x) (seq x)
       :else x))
   (fn [x children]
     (cond
       (map? x) (into {} (map #(into [] %) children))
       (set? x) (into #{} children)
       (vector? x) (into [] children)
       :else children))
   root))

(defn- invocation-of
  "Predicate returning whether x is a form
   representing an invocation of the function
   identified by symbol s."
  [x s]
  (and (seq? x)
       (= (first x) s)))

(defn replace-in-tree
  [prj f next]
  (loop [loc (any-zip f)]
    (if (z/end? loc)
      (e/right (z/root loc))
      (let [n (next (z/node loc))]
        (cond
          (contains? n :return)
            (:return n)
          (contains? n :replace-with)
            (recur (z/replace loc (:replace-with n)))
          :else (recur (z/next loc)))))))

(defn selective-eval
  "Given a form that contains sub-forms like
   (ulvm.core/eval (...)),
   returns a form with all eval invocations
   replaced with their values"
  [prj f]
  (replace-in-tree
    prj
    f
    #(if (invocation-of % 'ulvm.core/eval)
      {:replace-with (eval (second %))})))

(s/fdef selective-eval
        :args (s/cat :prj  ::project
                     :form su/any)
        :ret (su/either-of? su/any su/any))

(defn- resolve-env-ref
  [prj ctx node]
  (let [val (get-ctx-env prj ctx (rest node))]
    (if (e/right? val)
      (m/extract val)
      val)))

(defn resolve-env-refs
  "Given a form that contains sub-forms like
   (ulvm.core/from-env [:my-env-key]),
   returns a form with all from-env invocations
   replaced with their values"
  [prj ctx f]
  (replace-in-tree
    prj
    f
    #(if (invocation-of % 'ulvm.core/from-env)
      (let [val (resolve-env-ref prj ctx %)]
        (if (e/left? val)
          {:return val}
          {:replace-with val})))))

(s/fdef resolve-env-refs
        :args (s/cat :prj  ::project
                     :ctx  sequential?
                     :form su/any)
        :ret (su/either-of? su/any su/any))

(defn apply-bindings
  "Given a form that contains sub-forms that are
   symbol keys in the bindings map, replaces the
   references with their values"
  [prj bindings f]
  (replace-in-tree
    prj
    f
    #(if (and (symbol? %) (contains? bindings %))
      {:replace-with (core-get bindings %)})))

(s/fdef apply-bindings
        :args (s/cat :prj  ::project
                     :bindings map?
                     :form su/any)
        :ret (su/either-of? su/any su/any))

(defn eval-in-ctx
  "Resolves environment references and evaluates
   configuration in the provided context"
  ([prj ctx bindings cfg]
    (futil/mlet e/context
                [bound    (apply-bindings prj bindings cfg)
                 de-refed (resolve-env-refs prj ctx bound)
                 evaled   (selective-eval prj de-refed)]
                (e/right evaled)))
  ([prj ctx cfg]
    (eval-in-ctx prj ctx {} cfg)))

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
  (let [re-ref (::ucore/runnable-env-ref entity)
        {re-prj :prj, renv-map :el} (get-or-make prj :renvs re-ref entity)
        renv-from-renv-map (comp first vals ::ucore/runnable-envs)
        renv (m/fmap renv-from-renv-map renv-map)]
    {:prj re-prj
     :el renv}))

(s/fdef deref-runnable-env
        :args (s/cat :proj ::project
                     :entity (s/keys :req [::ucore/runnable-env-ref]))
        :ret (s/keys :req-un [::prj
                              ::el]))

(defn canonical-flow
  [flow-ent]
  (with-meta (s/conform ::ucore/flow flow-ent) (meta flow-ent)))
