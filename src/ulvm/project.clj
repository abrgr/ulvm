(ns ^{:author "Adam Berger"} ulvm.project
  "Project definition"
  (:require [clojure.spec :as s]
            [clojure.zip :as z]
            [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [ulvm.func-utils :as futil]
            [ulvm.env-keypaths :as k]
            [cats.monad.either :as e]
            [cats.core :as m])
  (:refer-clojure :rename {get core-get
                           set core-set}))

(declare get-runnable-env-rep
         block-with-results
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
  (-block-with-results
    [this prj config invocations body]
    "Generates an AST for a scope in which the results of
     the invocations are available and in which body 
     is included (body uses the results).")
  (-get-mod-combinator-config
    [this prj config]
    "Returns the config for this combinator."))

(defn block-with-results
  "Generates an AST for a scope in which the results of
   the invocations are available and in which body 
   is included (body uses the results)."
  [this prj config invocations body]
  (-block-with-results this prj config invocations body))

(defn get-mod-combinator-config
  "Returns the config for this combinator."
  [this prj config]
  (-get-mod-combinator-config this prj config))

(s/def ::mod ::ucore/module)

(s/def ::scope ::ucore/name)

(s/def ::inv map?) ; TODO: this map is the result of conforming a uprj/flow-invocation

(s/def ::mod-name symbol?)

(s/def ::result-names
  (s/map-of keyword? symbol?))

(s/def ::arg-names
  (s/map-of keyword? symbol?))

(s/def ::enhanced-invocation
  (s/keys :req-un [::scope
                   ::mod
                   ::inv
                   ::result-names
                   ::arg-names
                   ::mod-name]))

(s/fdef block-with-results
        :args (s/cat :combinator  #(satisfies? ModCombinator %)
                     :prj         ::project
                     :config      map?
                     :invocations (s/spec
                                    (s/+ ::enhanced-invocation))
                     :body        coll?)
        :ret e/either?)

(s/fdef get-mod-combinator-config
        :args (s/cat :combinator  #(satisfies? ModCombinator %)
                     :prj         ::project
                     :config      map?)
        :ret e/either?)

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
  ([prj ent-key]
   (get-in prj [:entities ent-key]))
  ([prj ent-key name]
    (get-in prj [:entities ent-key name])))

(defn get-or-make
  "Get or make a project element"
  [prj type id entity]
  (let [orig (get-in prj [type id])]
    (cond
      (some? orig) {:prj prj, :el orig}
      :else (futil/mlet e/context
                        [make    (maker-from-prj-type type)
                         ent     entity
                         new-prj (make prj id ent)]
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
                     :entity (s/nilable
                               (s/or :mc   ::ucore/mod-combinator
                                     :rel  ::ucore/runnable-env-loader
                                     :renv ::ucore/runnable-env)))
        :ret (s/keys :req-un [::prj ::el]))

(defn get
  "Get or create a project element"
  [prj type id]
  (futil/mlet e/context
              [ent-key (entity-type-from-prj-type type)
               ent     (get-prj-ent prj ent-key id)]
              (get-or-make prj type id ent)))

(s/fdef get
        :args (s/cat :prj  ::project
                     :type #{:env
                             :entities
                             :mod-combinators
                             :renv-loaders
                             :renvs}
                     :name keyword?)
        :ret (s/keys :req-un [::prj ::el]))

(defn- first-not-identical
  [sentinel not-found l]
  (if (empty? l)
    not-found
    (if (identical? sentinel (first l))
       (recur sentinel not-found (rest l))
       (first l))))

(defn get-ctx-env
  "Get an environment value, by checking every key in
   the prj environment in the ordered collection of keys:
   (first ctxs) + key-path, ..., (last ctxs) + key-path,
   key-path"
  ([prj ctxs key-path]
   (get-ctx-env prj ctxs key-path nil))
  ([prj ctxs key-path not-found]
   (let [sentinel (Object.)]
          ; our contexts (ending in a root context)
     (->> (concat ctxs [[]])
          ; our full key-paths (including context)
          (map #(concat % key-path))
          ; our env values (eithers) for each full key-path
          (map #(get-env prj % sentinel))
          ; an either of a list of values (instead of list of eithers)
          (reduce
            (fn [acc item]
              (m/mlet [a acc
                       i item]
                (e/right (conj a i))))
            (e/right []))
          ; either the first non-sentinel or not-found
          (m/fmap (partial first-not-identical sentinel not-found))))))

(s/fdef get-ctx-env
        :args (s/cat :prj ::project
                     :ctx (s/coll-of sequential?)
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
  [f next]
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
  [f]
  (replace-in-tree
    f
    #(if (invocation-of % 'ulvm.core/eval)
      {:replace-with (eval (second %))})))

(s/fdef selective-eval
        :args (s/cat :form su/any)
        :ret (su/either-of? su/any su/any))

(defn- resolve-env-ref
  [prj ctxs node]
  (let [val (get-ctx-env prj ctxs (rest node))]
    (if (e/right? val)
      (m/extract val)
      val)))

(defn resolve-env-refs
  "Given a form that contains sub-forms like
   (ulvm.core/from-env [:my-env-key]),
   returns a form with all from-env invocations
   replaced with their values"
  [prj ctxs f]
  (replace-in-tree
    f
    #(if (invocation-of % 'ulvm.core/from-env)
      (let [val (resolve-env-ref prj ctxs %)]
        (if (e/left? val)
          {:return val}
          {:replace-with val})))))

(s/fdef resolve-env-refs
        :args (s/cat :prj  ::project
                     :ctx  (s/coll-of sequential?)
                     :form su/any)
        :ret (su/either-of? su/any su/any))

(defn apply-bindings
  "Given a form that contains sub-forms that are
   symbol keys in the bindings map, replaces the
   references with their values"
  [bindings f]
  (replace-in-tree
    f
    #(if (and (symbol? %) (contains? bindings %))
      {:replace-with (core-get bindings %)})))

(s/fdef apply-bindings
        :args (s/cat :bindings map?
                     :form su/any)
        :ret (su/either-of? su/any su/any))

(defn eval-in-ctx
  "Resolves environment references and evaluates
   configuration in the provided context"
  ([prj ctxs bindings cfg]
    (futil/mlet e/context
                [bound    (apply-bindings bindings cfg)
                 de-refed (resolve-env-refs prj ctxs bound)
                 evaled   (selective-eval de-refed)]
                (e/right evaled)))
  ([prj ctxs cfg]
    (eval-in-ctx prj ctxs {} cfg)))

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

(defn init
  "Initial project"
  [entities env]
  {:entities        entities
   :mod-combinators {}
   :renv-loaders    {}
   :renvs           {}
   :env             env})
