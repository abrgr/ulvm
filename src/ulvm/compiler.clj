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
            [ulvm.flows :as flows]
            [ulvm.utils :as u]
            [ulvm.env-keypaths :as k]
            [ulvm.scopes :as scopes]
            [ulvm.blocks :as b]
            [ulvm.env :as env]
            [ulvm.fileserver :as fs]
            [clojure.set :as cset]
            [clojure.java.io :as io]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare ulvm-compile
         build-scopes
         build-scope
         get-mod-combinators
         make-scope
         gen-ast
         gen-block-ast)

(defn ulvm-compile
  "Compile a ulvm system"
  [cmd-line-env project-dir]
  (let [env             (env/resolve-initial-env cmd-line-env project-dir)
        ulvm-path       (-> project-dir
                            (io/file (get-in env (k/ulvm-root)))
                            (.getPath))
        ulvm-entities   (uread/read-ulvm-dir ulvm-path)
        empty-project   (uprj/init ulvm-entities env)
        prj-with-scopes (build-scopes empty-project)]
    prj-with-scopes))

(defn- make-scopes
  [prj]
  (futil/mlet e/context
              [mcs (get-mod-combinators prj)]
    (let [{:keys [prj
                  mod-combinators
                  mod-combinator-cfgs]} mcs]
      (->> (get-in prj [:entities ::ucore/scopes])
           (reduce
            (fn [info [scope-name scope-ent]]
              (let [{:keys [prj
                            mods
                            scope]} (make-scope (:prj info) scope-name scope-ent)]
                {:prj    prj
                 :mods   (-> (:mods info) (merge {scope-name mods}))
                 :scopes (-> (:scopes info) (merge {scope-name scope}))}))
            {:prj    prj
             :mods   {}
             :scopes {}})
           (merge {:mod-combinators     mod-combinators
                   :mod-combinator-cfgs mod-combinator-cfgs})))))

(defn- expand-transformers
  [prj mods scopes]
  (let [named-scope-cfgs (->> scopes
                              (reduce
                               (fn [named-scope-cfgs [scope-name scope]]
                                 (assoc named-scope-cfgs scope-name (scopes/get-config scope)))
                               {}))
        scope-inits      (->> scopes
                              (reduce
                               (fn [flow-by-name [scope-name _]]
                                 (let [scope (uprj/get-prj-ent prj ::ucore/scopes scope-name)
                                       init  (get scope ::ucore/init)
                                       f     (->> init
                                                  (flows/invs-in-scope scope-name)
                                                  (concat [{::home-scope scope-name}])
                                                  (into []))
                                       n     (keyword (str "*init-" (name scope-name) "*"))]
                                   (if (some? init)
                                       (assoc flow-by-name n f)
                                       flow-by-name)))
                               {}))]
    (->> (uprj/get-prj-ent prj ::ucore/flows)
         (merge scope-inits) ; TODO: check uniqueness
         (reduce
          (fn [acc [flow-name flow]]
            (let [canonical-flow (uprj/canonical-flow flow)
                  expanded (flows/expand-transformers prj mods named-scope-cfgs canonical-flow)]
              (merge-with
               merge
               acc
               {:extra-mods-by-scope (get expanded :extra-mods-by-scope)
                :canonical-flows     {flow-name (get expanded :canonical-flow)}})))
          {}))))

(defn- build-scopes
  "Builds all scopes"
  [proj]
  (let [{:keys [prj
                mods
                scopes
                mod-combinators
                mod-combinator-cfgs]} (make-scopes proj)
        {:keys [extra-mods-by-scope
                canonical-flows]}     (expand-transformers prj mods scopes)]
    (fs/with-fs prj
      (reduce
       (fn [prj [scope-name scope-ent]]
         (build-scope
           prj
           mod-combinators
           mod-combinator-cfgs
           (merge-with merge mods extra-mods-by-scope) ; TODO: ensure uniqueness
           scope-name
           (get scopes scope-name)
           canonical-flows))
       prj
       (get-in prj [:entities ::ucore/scopes])))))

(defn- scope-mod-descs
  [mods-by-name]
  (->> mods-by-name
       vals
       (map ::ucore/mod-descriptor)
       (into #{})))

(defn- scope-modules
  "Return a map from module names to their representations"
  [prj scope scope-ent scope-name]
  (let [implicits (scopes/get-implicit-modules scope prj)
        explicits (-> (uprj/get-prj-ent prj ::ucore/scopes scope-name)
                      (get ::ucore/modules))
        all-mods  (merge implicits explicits)]
    (->> all-mods
         (map
           (fn [[n m]]
             [n
              (->>
                (scopes/get-module-config
                  scope
                  prj
                  (::ucore/mod-descriptor m)
                  (get m ::ucore/config {}))
                (assoc m ::ucore/config))]))
         (into {}))))

(defn- resolve-name
  [prj scope & name-parts]
  (->> name-parts
       (map name)
       (scopes/resolve-name scope prj)))

(defn- enhance-invocations
  [prj mods scope-name scope invs]
  (->> invs
       (map
         (fn [[n inv]]
           [n 
            (-> (flows/module-for-invocation mods scope-name inv)
                (update
                 :mod-name ; TODO: should probably name this something else
                 (fn [mod-name]
                   (->> mod-name
                        str
                        (resolve-name prj scope))))
                (merge {:inv inv}))]))
       (filter
         #(= scope-name (:scope (second %))))
       (into {})))

(defn- get-sub-results
  [enhanced-invs inverse-deps inv-name]
  (->> (get inverse-deps inv-name)
       (map #(get-in enhanced-invs [% :inv :args]))
       (map vals)
       (apply concat)
       (map #(u/get-in % [:ref :named-ref-arg :sub-result]))
       (filter some?)
       (concat [:*default*])))

(defn- get-sub-result-names
  [prj scope-name scope enhanced-invs inverse-deps inv-name inv]
  (let [result-name (or (u/get-in inv [:inv :name :name])
                        (some-> (u/get-in inv [:mod-name])
                                (str "-")
                                gensym))]
    (->> (get-sub-results enhanced-invs inverse-deps inv-name)
         (u/map-to-vals
           (partial
             resolve-name
             prj
             scope
             result-name)))))

(defn- add-result-names
  "Get the usages for the result of each inv, get the names for
   them, and annotate the inv with its result names."
  [prj scope-name scope inverse-deps enhanced-invs]
  (->> enhanced-invs
       (map
         (fn [[n inv]]
           [n
            (merge 
              inv
              {:result-names
               (get-sub-result-names
                 prj
                 scope-name
                 scope
                 enhanced-invs
                 inverse-deps
                 n
                 inv)})]))
       (into {})))

(defn- add-arg-names
  "Resolve the names of the arguments to the invocations."
  [flow-params enhanced-invs]
  (->> enhanced-invs
       (map
         (fn [[n inv]]
           [n
            (merge 
              inv
              {:arg-names
               (->> (u/get-in inv [:inv :args])
                    ; get a list of nils or {:arg :res :sub-res}
                    (map
                      (fn [[arg-name arg]]
                        (let [def-ref-arg   (u/get-in arg [:ref :default-ref-arg])
                              named-ref-arg (u/get-in arg [:ref :named-ref-arg])]
                          (cond
                            (contains? flow-params def-ref-arg)
                            {:arg arg-name
                             :resolved def-ref-arg}
                            (some? def-ref-arg)
                            {:arg arg-name
                             :res def-ref-arg
                             :sub :*default*}
                            (some? named-ref-arg)
                            {:arg arg-name
                             :res (u/get-in named-ref-arg [:result])
                             :sub (u/get-in named-ref-arg [:sub-result])}))))
                    (filter some?)
                    ; get [[arg arg-name] ...]
                    (map
                      (fn [{:keys [arg res sub resolved]}]
                        [arg
                         (if (some? resolved)
                           resolved
                           (get-in
                             enhanced-invs
                             [res :result-names sub]))]))
                    ; get {arg arg-name, ...}
                    (into {}))})]))
       (into {})))

(defn- gen-block-ast
  [prj graph-cfg mod-combinators flow-params block]
  (let [{:keys [provides
                invs
                depends-on
                body]}     block
        body-ast           (gen-ast prj graph-cfg mod-combinators flow-params body)
        mc-name            (->> invs
                                first  ; the first [name, inv]
                                second ; the inv
                                :mod
                                ::ucore/mod-combinator-name)
        mc                 (get mod-combinators mc-name)
        mc-cfg             (get-in graph-cfg [:mod-combinator-cfgs mc-name])]
    (if (cset/subset? provides flow-params)
      ; we have no invocation for the flow-params
      body-ast
      (m/bind
        body-ast
        #(uprj/block-with-results
          mc
          prj
          mc-cfg
          (vals invs)
          %)))))

(defn- gen-ast
  "Generate an AST given a block graph"
  [prj graph-cfg mod-combinators flow-params blocks]
  (reduce
    (fn [acc-either block]
      (m/mlet [acc       acc-either
               block-ast (gen-block-ast
                           prj
                           graph-cfg
                           mod-combinators
                           flow-params
                           block)]
        (e/right (concat acc block-ast))))
    (e/right [])
    blocks))

(defn- build-flow-in-scope
  "Builds the portion of a flow contained in a scope"
  [prj graph-cfg mod-combinators mods scope-name scope flow-name flow]
  (let [invs          (->> (get flow :invocations)
                           flows/named-invocations)
        deps          (flows/invocation-dependency-graph invs)
        inverse-deps  (u/flip-map deps)
        enhanced-invs (enhance-invocations prj mods scope-name scope invs)
        ; TODO: this is wrong - we need something more
        ;       intelligent to split a flow into steps for a scope
        relevant-invs (->> invs
                           (filter
                             #(= (get-in enhanced-invs [(key %) :scope]) scope-name))
                           (into {}))
        flow-params        (flows/flow-params flow)
        flow-params-set    (set flow-params)
        named-invs         (->> enhanced-invs
                                (add-result-names
                                  prj
                                  scope-name
                                  scope
                                  inverse-deps)
                                (add-arg-names flow-params-set))
        resolved-flow-name   (resolve-name prj scope flow-name)]
    (m/->>= (flows/ordered-invocations relevant-invs deps flow-params)
            (b/build-call-graph
              prj
              scope-name
              scope
              flow-name
              deps
              inverse-deps
              graph-cfg
              named-invs)
            (gen-ast prj graph-cfg mod-combinators flow-params-set)
            (scopes/write-flow
              scope
              prj
              resolved-flow-name
              flow-params
              (uprj/get-prj-ent prj ::ucore/flow flow-name)))))

(s/fdef build-flow-in-scope
        :args (s/cat :prj        ::uprj/project
                     :graph-cfg  (s/keys :req-un [::mod-combinator-cfgs])
                     :mcs        (s/map-of keyword? #(satisfies? uprj/ModCombinator %))
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
  [proj graph-cfg mod-combinators mods scope-name scope canonical-flows]
  (->> canonical-flows
       (reduce
         (fn [prj [flow-name canonical-flow]]
            (->> canonical-flow
                 (build-flow-in-scope prj graph-cfg mod-combinators mods scope-name scope flow-name)
                 (uprj/set-env prj (k/build-flow-in-scope-keypath scope-name flow-name))))
         proj)))

(s/def ::mod-combinator-cfgs
  (s/map-of keyword? map?))

(s/fdef build-flows-in-scope
        :args (s/cat :prj             ::uprj/project
                     :graph-cfg       (s/keys :req-un [::mod-combinator-cfgs])
                     :mcs             (s/map-of keyword? #(satisfies? uprj/ModCombinator %))
                     :mods            (s/map-of
                                       keyword?
                                       (s/map-of keyword?
                                                 ::ucore/module))
                     :scope-name      keyword?
                     :scope           #(satisfies? scopes/Scope %)
                     :canonical-flows (s/map-of keyword? map?))
        :ret  ::uprj/project)

(defn- get-mod-combinators
  "Gets mod combinators, launches them, and gets their configs"
  [proj]
  (let [p-mcs (reduce
                (fn [acc [mc-name mc-ent]]
                  (m/bind acc
                    (fn [{:keys [prj mcs mc-cfgs]}]
                      (let [{p    :prj
                             mc-e :el} (uprj/get prj :mod-combinators mc-name)
                            orig-cfg (get mc-ent ::ucore/config {})]
                        (m/bind mc-e
                          (fn [mc]
                            (e/right
                              {:prj     p
                               :mcs     (assoc mcs mc-name mc)
                               :mc-cfgs (assoc mc-cfgs mc-name (uprj/get-mod-combinator-config mc p orig-cfg))})))))))
                (e/right {:prj     proj
                          :mcs     {}
                          :mc-cfgs {}})
                (uprj/get-prj-ent proj ::ucore/mod-combinators))]
      (m/bind p-mcs
              (fn [{:keys [prj mcs mc-cfgs]}]
                {:prj                 prj
                 :mod-combinators     mcs
                 :mod-combinator-cfgs mc-cfgs}))))

(defn- make-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (->
    (futil/mlet e/context
                [res   (scopes/make-scope proj scope-name scope-ent)
                 scope (:scope res)
                 prj   (:prj res)
                 mods  (scope-modules prj scope scope-ent scope-name)]
      {:prj   (uprj/set-env prj (k/make-scope-keypath scope-name) :success)
       :mods  mods
       :scope scope})
    (futil/recover-with 
      (fn [v]
        {:prj   (uprj/set-env proj (k/make-scope-keypath scope-name) v)
         :mods  {}
         :scope nil}))))

(s/fdef make-scope
        :args (s/cat :prj        ::uprj/project
                     :scope-name keyword?
                     :scope      ::ucore/scope)
        :ret  map?)

(defn- build-scope
  "Builds a scope"
  [prj mod-combinators mod-combinator-cfgs mods scope-name scope canonical-flows]
  (->
    (futil/mlet e/context
                [graph-cfg {:mod-combinator-cfgs mod-combinator-cfgs}
                 built-prj (build-flows-in-scope prj graph-cfg mod-combinators mods scope-name scope canonical-flows)]
                (uprj/set-env
                  built-prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    built-prj
                    (scope-mod-descs (get mods scope-name)))))
    (futil/recover-with #(uprj/set-env prj (k/scope-deps-keypath scope-name) %))))
