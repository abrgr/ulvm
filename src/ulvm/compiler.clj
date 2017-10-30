(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.reader :as uread]
            [ulvm.runnable-envs :as renv]
            [ulvm.runners]
            [ulvm.re-loaders]
            [ulvm.mod-combinators]
            [ulvm.func-utils :as futil]
            [ulvm.utils :as u]
            [ulvm.env-keypaths :as k]
            [ulvm.scopes :as scopes]
            [ulvm.blocks :as b]
            [clojure.set :as cset]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare ulvm-compile
         build-scopes
         build-scope
         make-scope
         gen-ast
         gen-block-ast)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (let [ulvm-entities   (uread/read-ulvm-dir directory)
        env             {::ucore/project-root directory} ; TODO: get a real environment
        empty-project   (uprj/init ulvm-entities env)
        prj-with-scopes (build-scopes empty-project)]
    prj-with-scopes)) ; TODO

(defn- make-scopes
  [prj]
  (reduce
   (fn [info [scope-name scope-ent]]
     (let [{:keys [prj
                   mods
                   mod-combinators
                   mod-combinator-cfgs
                   scope
                   scope-cfg]} (make-scope (:prj info) scope-name scope-ent)]
       {:prj                 prj
        :mods                (-> (:mods info) (merge {scope-name mods}))
        :mod-combinators     (-> (:mod-combinators info) (merge {scope-name mod-combinators}))
        :mod-combinator-cfgs (-> (:mod-combinator-cfgs info) (merge {scope-name mod-combinator-cfgs}))
        :scopes              (-> (:scopes info) (merge {scope-name scope}))
        :scope-cfgs          (-> (:scope-cfg info) (merge {scope-name scope-cfg}))}))
   {:prj                 prj
    :mods                {}
    :mod-combinators     {}
    :mod-combinator-cfgs {}
    :scopes              {}
    :scope-cfgs          {}}
   (get-in prj [:entities ::ucore/scopes])))

(defn- build-scopes
  "Builds all scopes"
  [proj]
  (let [{:keys [prj
                mods
                scopes
                scope-cfgs
                mod-combinators
                mod-combinator-cfgs]} (make-scopes proj)]
    (reduce
     (fn [prj [scope-name scope-ent]]
       (build-scope
         prj
         mod-combinators
         mod-combinator-cfgs
         mods
         scope-name
         (get scope-cfgs scope-name)
         (get scopes scope-name)))
     prj
     (get-in prj [:entities ::ucore/scopes]))))

(defn- scope-mod-descs
  [mods-by-name]
  (->> mods-by-name
       vals
       (map ::ucore/mod-descriptor)
       (into #{})))

(defn- default-invocation-name
  [invocation]
  (-> (get invocation :invocation-module)
      (#(or (get-in % [:scope-module :module-name])
            (get    % :local-module)))
      (str "_")
      (gensym)))

(defn- invocation-name
  "Returns the name for the result of an invocation"
  [invocation]
  (get-in
    invocation
    [:name :name]
    (default-invocation-name invocation)))

(defn- named-invocations
  "Returns a map from result names to invocations"
  [invocations]
  (reduce
    (fn [named inv]
      (conj named [(invocation-name inv) inv]))
    {}
    invocations))

(defn- invocation-deps
  [invocation]
  (cset/union
    (->>
      (get-in invocation [:after :names])
      second
      flatten
      (filter identity)
      (into #{}))
    (->> (:args invocation)
      (filter
        (fn [[_ [arg-type arg]]]
          (= :ref arg-type)))
      (map
        (fn [[_ [_ [ref-arg-type arg]]]]
          (if (= ::ucore/default-ref-arg ref-arg-type)
            arg
            (:result arg))))
      (into #{}))))

(defn- invocation-dependency-graph
  "Returns a map from invocation name to a set of names of the invocations it depends on."
  [invocations]
  (reduce
    (fn [deps [name invocation]]
      (merge-with cset/union deps {name (invocation-deps invocation)}))
    {}
    invocations))

(defn- ordered-invocations
  "Returns a seq of topologically ordered invocation names"
  [invs deps args]
  (let [sorted  (u/topo-sort deps (keys invs) (into #{} args))]
    (if (empty? (:unsat sorted))
        (e/right (:items sorted))
        (e/left  {:msg        "Failed to satisfy call graph dependencies"
                  :unsat-deps (:unsat sorted)}))))

(defn- scope-modules
  "Return a map from module names to their representations"
  [prj scope scope-ent cfg scope-name]
  (let [implicits (scopes/get-implicit-modules scope prj cfg)
        explicits (-> (uprj/get-prj-ent prj ::ucore/scopes scope-name)
                      (get ::ucore/modules))
        all-mods  (merge implicits explicits)]
    (->>
      (map
        (fn [[n m]]
          [n
           (->>
             (scopes/get-module-config
               scope
               prj
               (::ucore/mod-descriptor m)
               (get m ::ucore/config {}))
             (assoc m ::ucore/config))])
        all-mods)
      (into {}))))

(defn- module-for-invocation
  "The module that the given invocation references.
   A scope-name is used to disambiguate references to
   local modules."
  [mods scope-name inv]
  (let [inv-mod     (->> [(:invocation-module inv)]
                         (into {}))
        scope-mod   (:scope-module inv-mod)
        local-mod   (:local-module inv-mod)
        ; we have to do some gymnastics to handle local and
        ; scope modules
        scope       (-> (or (:scope scope-mod)
                             scope-name)
                        keyword)
        module-name (-> (or (:module-name scope-mod)
                             local-mod)
                        keyword)
        module      (get-in mods [scope module-name])]
    {:mod   module
     :scope scope}))

(defn- enhance-invocations
  [mods scope-name invs]
  (->> invs
       (map
         (fn [[n inv]]
           [n 
            (-> (module-for-invocation mods scope-name inv)
                (merge {:inv inv}))]))
       (filter
         #(= scope-name (:scope (second %))))
       (into {})))

(defn- gen-block-ast
  [block]
  ; TODO: this needs to interact with module combinators
  (let [names    (get block :provides)
        bindings (->> names
                      (map #(str "invoke-" %))
                      (interleave (map symbol names))
                      (into []))
        body     (gen-ast (get block :body))]
    `(~'let ~bindings (do ~@body))))

(defn- gen-ast
  "Generate an AST given a block graph"
  [blocks]
  (map gen-block-ast blocks))

(defn- build-flow-in-scope
  "Builds the portion of a flow contained in a scope"
  [proj graph-cfg mods scope-name scope flow-name flow]
  (let [invs          (->> (get flow :invocations)
                           named-invocations)
        enhanced-invs (enhance-invocations mods scope-name invs)
        ; TODO: this is wrong - we need something more
        ;       intelligent to split a flow into steps for a scope
        relevant-invs (->> invs
                           (filter
                             #(= (get-in enhanced-invs [(key %) :scope]) scope-name))
                           (into {}))
        deps          (invocation-dependency-graph invs)]
    (m/->>= (ordered-invocations relevant-invs deps (::ucore/args (meta flow)))
            (b/build-call-graph proj scope flow-name deps graph-cfg enhanced-invs)
            (#(e/right (gen-ast %))))))

(s/fdef build-flow-in-scope
        :args (s/cat :prj        ::uprj/project
                     :graph-cfg  (s/keys :req-un [::mod-combinator-cfgs])
                     :mods       (s/map-of keyword?
                                           (s/map-of keyword?
                                                     ::ucore/module))
                     :scope-name keyword?
                     :scope      #(satisfies? scopes/Scope %)
                     :flow-name  keyword?
                     :flow       map?)
        :ret  ::uprj/project)

(defn- build-flows-in-scope
  "Builds all flows for the given scope."
  [proj graph-cfg mods scope-name scope]
  (->> (get-in proj [:entities ::ucore/flows])
       (reduce
         (fn [prj [flow-name flow]]
            (->> flow
                 (uprj/canonical-flow)
                 (build-flow-in-scope prj graph-cfg mods scope-name scope flow-name)
                 (uprj/set-env prj (k/build-flow-in-scope-keypath scope-name flow-name))))
         proj)))

(s/def ::mod-combinator-cfgs
  (s/map-of keyword? map?))

(s/fdef build-flows-in-scope
        :args (s/cat :prj        ::uprj/project
                     :graph-cfg  (s/keys :req-un [::mod-combinator-cfgs])
                     :mods       (s/map-of keyword?
                                           (s/map-of keyword?
                                                     ::ucore/module))
                     :scope-name keyword?
                     :scope      #(satisfies? scopes/Scope %))
        :ret  ::uprj/project)

(defn- make-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (->
    (futil/mlet e/context
                [res   (scopes/make-scope proj scope-ent)
                 scope (:scope res)
                 prj   (:prj res)
                 cfg   (scopes/get-config scope prj (get scope-ent ::ucore/config))
                 mods  (scope-modules prj scope scope-ent cfg scope-name)
                 p-mcs (reduce
                         (fn [{:keys [prj mcs]} [_ mod]]
                           (let [mc-name  (::ucore/mod-combinator-name mod)
                                 {p  :prj
                                  mc :el} (uprj/get prj :mod-combinators mc-name)]
                             {:prj p
                              :mcs (assoc mcs mc-name mc)}))
                         {:prj prj
                          :mcs {}}
                         mods)
                 p       (:prj p-mcs)
                 mcs     (:mcs p-mcs)
                 mc-cfgs (->>
                           (map
                             (fn [[n mc]]
                               (let [mc-ent   (uprj/get-prj-ent p ::ucore/mod-combinators n)
                                     orig-cfg (get mc-ent ::ucore/config {})
                                     mc-cfg   (m/bind mc #(uprj/get-mod-combinator-config % p orig-cfg))]
                                 [n mc-cfg]))
                             mcs)
                           (into {}))]
      
      {:prj                 (uprj/set-env p (k/make-scope-keypath scope-name) :success)
       :mods                mods
       :mod-combinators     mcs
       :mod-combinator-cfgs mc-cfgs
       :scope               scope
       :scope-cfg           cfg})
    (futil/recover-with 
      (fn [v]
        {:prj   (uprj/set-env proj (k/make-scope-keypath scope-name) v)
         :mods                {}
         :mod-combinators     {}
         :mod-combinator-cfgs {}
         :scope               nil
         :scope-cfg           nil}))))

(s/fdef make-scope
        :args (s/cat :prj        ::uprj/project
                     :scope-name keyword?
                     :scope      ::ucore/scope)
        :ret  map?)

(defn- build-scope
  "Builds a scope"
  [prj mod-combinators mod-combinator-cfgs mods scope-name scope-cfg scope]
  (->
    (futil/mlet e/context
                [graph-cfg {:mod-combinator-cfgs mod-combinator-cfgs}
                 built-prj (build-flows-in-scope prj graph-cfg mods scope-name scope)]
                (uprj/set-env
                  built-prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    built-prj
                    (scope-mod-descs mods))))
    (futil/recover-with #(uprj/set-env prj (k/scope-deps-keypath scope-name) %))))
