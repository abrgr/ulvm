(ns ^{:author "Adam Berger"} ulvm.flows
  "Flow operations"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.utils :as u]
            [ulvm.project :as uprj]
            [clojure.set :as cset]
            [cats.monad.either :as e]))

(defn module-for-invocation
  "The module that the given invocation references.
   A scope-name is used to disambiguate references to
   local modules."
  [mods scope-name inv]
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
    {:mod      module
     :mod-name module-name
     :scope    mod-scope-name}))

(s/fdef module-for-invocation
        :args (s/cat :mods       (s/map-of keyword?
                                           (s/map-of keyword?
                                                     ::ucore/module))
                     :scope-name (s/nilable keyword?)
                     :inv        ::uprj/inv)
        :ret (s/keys :req-un [::uprj/mod ::uprj/mod-name ::uprj/scope]))

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

(defn named-invocations
  "Returns a map from result names to invocations"
  [invocations]
  (reduce
    (fn [named inv]
      (conj named [(invocation-name inv) inv]))
    {}
    invocations))

(s/fdef named-invocations
        :args (s/cat :invocations (s/coll-of ::uprj/inv))
        :ret  (s/map-of symbol? ::uprj/inv))

(defn- relevant-transformers
  [prj bindings transformers]
  (reduce
   (fn [relevant [transform-name transform]]
     (let [phase (u/get-in transform [:when :phase])
           pred  (u/get-in transform [:when :if])]
       (if (uprj/eval-in-ctx prj [] bindings pred)
         (assoc-in relevant [phase] (u/get-in transform [:do])) ; TODO: do we really want to allow multiple transformers with the same phase?
         relevant)))
   {}
   transformers))

(s/def ::client-pre ::ucore/flow-invocations)
(s/def ::client-post ::ucore/flow-invocations)
(s/def ::server-pre ::ucore/flow-invocations)
(s/def ::server-post ::ucore/flow-invocations)

(s/def ::transformer-bindings map?)

(s/def ::transformers (s/map-of ::ucore/name ::ucore/transformer))

(s/fdef relevant-transformers
        :args (s/cat :prj          ::uprj/project
                     :bindings     ::transformer-bindings
                     :transformers (s/nilable ::transformers))
        :ret  (s/keys :req-un [::client-pre
                               ::client-post
                               ::server-pre
                               ::server-post]))

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

(defn invocation-dependency-graph
  "Returns a map from invocation name to a set of names of the invocations it depends on."
  [invocations]
  (reduce
    (fn [deps [name invocation]]
      (merge-with cset/union deps {name (invocation-deps invocation)}))
    {}
    invocations))

(defn ordered-invocations
  "Returns a seq of topologically ordered invocation names"
  [invs deps params]
  (let [sorted  (u/topo-sort deps (keys invs) (into #{} params))]
    (if (empty? (:unsat sorted))
        (e/right (:items sorted))
        (e/left  {:msg        "Failed to satisfy call graph dependencies"
                  :unsat-deps (:unsat sorted)}))))

(defn flow-params
  "Returns the parameters of the flow."
  [flow]
  (get (meta flow) ::ucore/args))

(defn- home-scope
  "Every flow has a \"home scope,\" which is the scope in which the flow
   originates and from which all invocations initiate."
  [flow]
  (u/get-in flow [:config ::ucore/home-scope]))

(defn- inv-with-default-scope
  "If inv refers to a local module, return an inv for that module on scope."
  [scope inv]
  (if-let [local-mod (u/get-in inv [:invocation-module :local-module])]
    (assoc
     inv
     :invocation-module
     {:scope-module (list (keyword local-mod) (symbol (str scope)))})
    inv))

(defn- invs-with-default-scope
  [scope invs]
  (->> invs
       (map
        (fn [[k inv]]
          [k (inv-with-default-scope scope inv)]))
       (into {})))

(defn- sym-in-ns
  [ns sym]
  (symbol (str "*" ns "*" sym)))

(defn- inv-in-ns
  [ns invs inv]
  (update
   inv
   :args
   #(->> %
         (map
          (fn [[arg-name arg]]
            (let [def-ref   (u/get-in arg [:ref :default-ref-arg])
                  named-ref (u/get-in arg [:ref :named-ref-arg])]
              (cond
               (and (some? def-ref) (contains? invs def-ref))
               [arg-name [:ref [:default-ref-arg (sym-in-ns ns def-ref)]]]
               (and (some? named-ref) (contains? invs (get named-ref :result)))
               [arg-name [:ref [:named-ref-arg (assoc named-ref :result (sym-in-ns ns (get named-ref :result)))]]]
               :else
               [arg-name arg]))))
         (into {}))))

(defn- invs-in-ns
  [ns invs]
  (reduce
   (fn [ns-invs [n inv]]
     (assoc
      ns-invs
      (sym-in-ns ns n)
      (inv-in-ns ns invs inv)))
   {}
   invs))

(defn- transformers-for-rel-scope
  [transformers rel-scope client-scope-name server-scope-name]
  (let [def-scope-by-phase {:client-pre  client-scope-name
                            :client-post client-scope-name
                            :server-pre  server-scope-name
                            :server-post server-scope-name}
        phases             (if (= :client rel-scope)
                               [:client-pre :client-post]
                               [:server-pre :server-post])]
    (map
     #(some->> (get transformers %)
               (s/conform ::ucore/flow-invocations)
               named-invocations
               (invs-with-default-scope (get def-scope-by-phase %)))
     phases)))

(defn- get-transformer-mods
  [mod transformers]
  (let [transformer-mods (->> transformers
                              (apply concat)
                              vals
                              (map #(first (u/get-in % [:invocation-module :scope-module])))
                              set)]
    (->> (get mod ::ucore/transformer-modules)
         (filter
          (fn [[mod-name mod]]
            (transformer-mods mod-name)))
         (into {}))))

(defn invs-in-scope
  "Place invs in the provided scope"
  [scope-name invs]
  (map
   (fn [[mod-ref & rest-of-inv]]
     (if (symbol? mod-ref)
         (concat [(list (-> mod-ref name keyword) (-> scope-name name symbol))] rest-of-inv)
         (concat [mod-ref] rest-of-inv))) ; TODO: can check that mod-ref refers to scope
   invs))

(s/fdef invs-in-scope
        :args (s/cat :scope-name keyword?
                     :invs       (s/nilable (s/coll-of ::ucore/flow-invocation)))
        :ret (s/coll-of ::ucore/flow-invocation))

(defn expand-transformers
  "Inlines applicable transformers for any module invocations in the flow."
  ([prj mods named-scope-cfgs canonical-flow]
    (expand-transformers prj mods named-scope-cfgs nil canonical-flow))
  ([prj mods named-scope-cfgs scope canonical-flow]
    (let [client-scope-name (home-scope canonical-flow)
          client-scope-cfg  (get named-scope-cfgs client-scope-name)]
      (->> canonical-flow
           :invocations
           named-invocations
           (reduce
             (fn [acc [inv-name inv]]
               (let [{mod               :mod
                      server-scope-name :scope} (module-for-invocation mods scope inv) ; TODO: if scope is not nil and server-scope-name != scope, we have a problem (this is the case for scope initializers)
                     server-scope-cfg           (get named-scope-cfgs server-scope-name)
                     param-bindings             (->> (u/get-in inv [:args])
                                                     (map
                                                      (fn [[k arg]]
                                                        [k (s/unform ::ucore/flow-arg arg)]))
                                                     (into {}))
                     transformer-bindings       {'*client-scope*     client-scope-name
                                                 '*client-scope-cfg* client-scope-cfg
                                                 '*server-scope*     server-scope-name
                                                 '*server-scope-cfg* server-scope-cfg
                                                 '*inv*              (u/get-in inv [:invocation-module])}
                     ; TODO: only allow some bindings in some phases
                     transformers               (->> (get mod ::ucore/transformers)
                                                     (relevant-transformers prj (merge transformer-bindings param-bindings)))
                     client-transformers        (transformers-for-rel-scope
                                                 transformers
                                                 :client
                                                 client-scope-name
                                                 server-scope-name)
                     server-transformers        (transformers-for-rel-scope
                                                 transformers
                                                 :server
                                                 client-scope-name
                                                 server-scope-name)
                     client-mods                (get-transformer-mods
                                                 mod
                                                 client-transformers)
                     server-mods                (get-transformer-mods
                                                 mod
                                                 server-transformers)
                     all-transformers           (->> (concat client-transformers server-transformers)
                                                     (apply merge)
                                                     (invs-in-ns inv-name))
                     new-invs                   (if (empty? all-transformers)
                                                    (list inv)
                                                    (vals all-transformers))
                     extra-mods-by-scope        (->> (merge
                                                      {server-scope-name server-mods}
                                                      {client-scope-name client-mods})
                                                     (filter #(some? (key %)))
                                                     (into {}))]
                 (-> acc
                     (update
                      :extra-mods-by-scope
                      #(merge-with merge % extra-mods-by-scope))
                     (update-in
                      [:canonical-flow :invocations]
                      #(concat % new-invs)))))
             {:extra-mods-by-scope {}
              :canonical-flow      (assoc canonical-flow :invocations [])})))))
