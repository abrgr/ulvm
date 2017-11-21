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
            [ulvm.env :as env]
            [clojure.set :as cset]
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
  (let [ulvm-entities   (uread/read-ulvm-dir project-dir)
        env             (env/resolve-initial-env cmd-line-env project-dir)
        empty-project   (uprj/init ulvm-entities env)
        prj-with-scopes (build-scopes empty-project)]
    prj-with-scopes)) ; TODO

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
                            scope
                            scope-cfg]}           (make-scope (:prj info) scope-name scope-ent)]
                {:prj                 prj
                 :mods                (-> (:mods info) (merge {scope-name mods}))
                 :scopes              (-> (:scopes info) (merge {scope-name scope}))
                 :scope-cfgs          (-> (:scope-cfgs info) (merge {scope-name scope-cfg}))}))
            {:prj                 prj
             :mods                {}
             :scopes              {}
             :scope-cfgs          {}})
           (merge {:mod-combinators     mod-combinators
                   :mod-combinator-cfgs mod-combinator-cfgs})))))

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
          (if (= :default-ref-arg ref-arg-type)
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
    (->> all-mods
         (map
           (fn [[n m]]
             [n
              (->>
                (scopes/get-module-config
                  scope
                  prj
                  cfg
                  (::ucore/mod-descriptor m)
                  (get m ::ucore/config {}))
                (assoc m ::ucore/config))]))
         (into {}))))

(defn- resolve-name
  [prj scope cfg & name-parts]
  (->> name-parts
       (map name)
       (scopes/resolve-name scope prj cfg)))

(defn- module-for-invocation
  "The module that the given invocation references.
   A scope-name is used to disambiguate references to
   local modules."
  [prj mods scope-name scope scope-cfg inv]
  (let [inv-mod     (->> [(:invocation-module inv)]
                         (into {}))
        scope-mod      (:scope-module inv-mod)
        local-mod      (:local-module inv-mod)
        ; we have to do some gymnastics to handle local and
        ; scope modules
        mod-scope-name (-> (or (:scope scope-mod)
                               scope-name)
                           keyword)
        module-name    (-> (or (:module-name scope-mod)
                               local-mod)
                           keyword)
        module         (get-in mods [mod-scope-name module-name])]
    {:mod          module
     :mod-name     (->> module-name
                        str
                        (resolve-name prj scope scope-cfg))
     :scope        mod-scope-name}))

(defn- enhance-invocations
  [prj mods scope-name scope scope-cfg invs]
  (->> invs
       (map
         (fn [[n inv]]
           [n 
            (-> (module-for-invocation prj mods scope-name scope scope-cfg inv)
                (merge {:inv         inv}))]))
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
  [prj scope-name scope scope-cfg enhanced-invs inverse-deps inv-name inv]
  (->> (get-sub-results enhanced-invs inverse-deps inv-name)
       (u/map-to-vals
         (partial
           resolve-name
           prj
           scope
           scope-cfg
           (u/get-in inv [:inv :name :name])))))

(defn- add-result-names
  "Get the usages for the result of each inv, get the names for
   them, and annotate the inv with its result names."
  [prj scope-name scope scope-cfg inverse-deps enhanced-invs]
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
                 scope-cfg
                 enhanced-invs
                 inverse-deps
                 n
                 inv)})]))
       (into {})))

(defn- add-arg-names
  "Resolve the names of the arguments to the invocations."
  [flow-args enhanced-invs]
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
                            (contains? flow-args def-ref-arg)
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
  [prj graph-cfg mod-combinators flow-args block]
  (let [{:keys [provides
                invs
                depends-on
                body]}     block
        body-ast           (gen-ast prj graph-cfg mod-combinators flow-args body)
        mc-name            (->> invs
                                first  ; the first [name, inv]
                                second ; the inv
                                :mod
                                ::ucore/mod-combinator-name)
        mc                 (get mod-combinators mc-name)
        mc-cfg             (get-in graph-cfg [:mod-combinator-cfgs mc-name])]
    (if (cset/subset? provides flow-args)
      ; we have no invocation for the flow-args
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
  [prj graph-cfg mod-combinators flow-args blocks]
  (reduce
    (fn [acc-either block]
      (m/mlet [acc       acc-either
               block-ast (gen-block-ast
                           prj
                           graph-cfg
                           mod-combinators
                           flow-args
                           block)]
        (e/right (concat acc block-ast))))
    (e/right [])
    blocks))

(defn- build-flow-in-scope
  "Builds the portion of a flow contained in a scope"
  [proj graph-cfg mod-combinators mods scope-name scope scope-cfg flow-name flow]
  (let [invs          (->> (get flow :invocations)
                           named-invocations)
        deps          (invocation-dependency-graph invs)
        inverse-deps  (u/flip-map deps)
        enhanced-invs (enhance-invocations proj mods scope-name scope scope-cfg invs)
        ; TODO: this is wrong - we need something more
        ;       intelligent to split a flow into steps for a scope
        relevant-invs (->> invs
                           (filter
                             #(= (get-in enhanced-invs [(key %) :scope]) scope-name))
                           (into {}))
        flow-args          (::ucore/args (meta flow))
        flow-args-set      (set flow-args)
        named-invs         (->> enhanced-invs
                                (add-result-names
                                  proj
                                  scope-name
                                  scope
                                  scope-cfg
                                  inverse-deps)
                                (add-arg-names flow-args-set))]
    (m/->>= (ordered-invocations relevant-invs deps flow-args)
            (b/build-call-graph
              proj
              scope-name
              scope
              flow-name
              deps
              inverse-deps
              graph-cfg
              named-invs)
            (#(gen-ast proj graph-cfg mod-combinators flow-args-set %)))))

(s/fdef build-flow-in-scope
        :args (s/cat :prj        ::uprj/project
                     :graph-cfg  (s/keys :req-un [::mod-combinator-cfgs])
                     :mcs        (s/map-of keyword? #(satisfies? uprj/ModCombinator %))
                     :mods       (s/map-of keyword?
                                           (s/map-of keyword?
                                                     ::ucore/module))
                     :scope-name keyword?
                     :scope      #(satisfies? scopes/Scope %)
                     :scope-cfg  map?
                     :flow-name  keyword?
                     :flow       map?)
        :ret  ::uprj/project)

(defn- build-flows-in-scope
  "Builds all flows for the given scope."
  [proj graph-cfg mod-combinators mods scope-name scope scope-cfg]
  (->> (get-in proj [:entities ::ucore/flows])
       (reduce
         (fn [prj [flow-name flow]]
            (->> flow
                 (uprj/canonical-flow)
                 (build-flow-in-scope prj graph-cfg mod-combinators mods scope-name scope scope-cfg flow-name)
                 (uprj/set-env prj (k/build-flow-in-scope-keypath scope-name flow-name))))
         proj)))

(s/def ::mod-combinator-cfgs
  (s/map-of keyword? map?))

(s/fdef build-flows-in-scope
        :args (s/cat :prj        ::uprj/project
                     :graph-cfg  (s/keys :req-un [::mod-combinator-cfgs])
                     :mcs        (s/map-of keyword? #(satisfies? uprj/ModCombinator %))
                     :mods       (s/map-of keyword?
                                           (s/map-of keyword?
                                                     ::ucore/module))
                     :scope-name keyword?
                     :scope      #(satisfies? scopes/Scope %)
                     :scope-cfg  map?)
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
                 cfg   (scopes/get-config scope prj (get scope-ent ::ucore/config {}))
                 mods  (scope-modules prj scope scope-ent cfg scope-name)]
      {:prj                 (uprj/set-env prj (k/make-scope-keypath scope-name) :success)
       :mods                mods
       :scope               scope
       :scope-cfg           cfg})
    (futil/recover-with 
      (fn [v]
        {:prj   (uprj/set-env proj (k/make-scope-keypath scope-name) v)
         :mods                {}
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
                 built-prj (build-flows-in-scope prj graph-cfg mod-combinators mods scope-name scope scope-cfg)]
                (uprj/set-env
                  built-prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    built-prj
                    ; TODO: figure out directory mounts and
                    ;       re-mapping of filenames
                    (merge scope-cfg
                           {:ulvm.scopes/src-dir "src"})
                    (scope-mod-descs (get mods scope-name)))))
    (futil/recover-with #(uprj/set-env prj (k/scope-deps-keypath scope-name) %))))
