(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.reader :as uread]
            [ulvm.runnable-envs :as renv]
            [ulvm.runners]
            [ulvm.re-loaders]
            [ulvm.func-utils :as futil]
            [ulvm.utils :as u]
            [ulvm.env-keypaths :as k]
            [ulvm.scopes :as scopes]
            [ulvm.blocks :as b]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare ulvm-compile
         build-scopes
         build-scope
         make-scope)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (let [ulvm-entities (uread/read-ulvm-dir directory)
        env {} ; TODO: get a real environment
        empty-project {:entities        ulvm-entities
                       :mod-combinators {}
                       :renv-loaders    {}
                       :renvs           {}
                       :env             {::ucore/project-root directory}}
        prj-with-scopes (build-scopes empty-project)]
    prj-with-scopes)) ; TODO

(defn- make-scopes
  [prj]
  (reduce
   (fn [info [scope-name scope-ent]]
     (let [{:keys [prj mods scope scope-cfg]} (make-scope (:prj info) scope-name scope-ent)]
       {:prj        prj
        :mods       (-> (:mods info) (merge {scope-name mods}))
        :scopes     (-> (:scopes info) (merge {scope-name scope}))
        :scope-cfgs (-> (:scope-cfg info) (merge {scope-name scope-cfg}))}))
   {:prj        prj
    :mods       {}
    :scopes     {}
    :scope-cfgs {}}
   (get-in prj [:entities ::ucore/scopes])))

(defn- build-scopes
  "Builds all scopes"
  [proj]
  (let [{:keys [prj mods scopes scope-cfgs]} (make-scopes proj)]
    (reduce
     (fn [prj [scope-name scope-ent]]
       (build-scope
         prj
         (get mods scope-name)
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
  (concat
    (->>
      (get-in invocation [:after :names])
      (map (comp second list))
      (flatten)
      (filter identity))
    (->> (:args invocation)
      (filter
        (fn [[_ [arg-type arg]]]
          (= :ref arg-type)))
      (map
        (fn [[_ [_ [ref-arg-type arg]]]]
          (if (= ::ucore/default-ref-arg ref-arg-type)
            arg
            (:result arg)))))))

(defn- invocation-dependency-graph
  "Returns a map from invocation name to a set of names of the invocations it depends on."
  [invocations]
  (reduce
    (fn [deps [name invocation]]
      (merge-with (comp vec concat) deps {name (invocation-deps invocation)}))
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
  (let [inv-mod     (:invocation-module inv)
        scope-mod   (:scope-module inv-mod)
        local-mod   (:local-module inv-mod)
        ; we have to do some gymnastics to handle local and
        ; scope modules
        scope-name  (or (:scope scope-mod)
                        scope-name)
        module-name (or (:module-name scope-mod)
                        local-mod)
        module      (get-in mods [scope-name module-name])]
    module))

(defn- enhance-invocations
  [mods scope-name invs]
  (->> invs
       (map
         (fn [[n inv]]
           [n {:inv inv
               :mod (module-for-invocation mods scope-name inv)}]))
       (into {})))

(defn- build-flow-in-scope
  "Builds the portion of a flow contained in a scope"
  [proj mods scope-name scope flow-name flow]
  (let [invs         (->> (get flow :invocations)
                          named-invocations
                          (enhance-invocations mods scope-name))
        deps         (invocation-dependency-graph invs)
        graph-config {}]
    (m/->>= (ordered-invocations invs deps (::ucore/args (meta flow)))
            (b/build-call-graph proj scope flow-name deps graph-config invs))))

(defn- build-flows-in-scope
  "Builds all flows for the given scope."
  [proj mods scope-name scope]
  (->> (get-in proj [:entities ::ucore/flows])
       (reduce
         (fn [prj [flow-name flow]]
            (->> flow
                 (uprj/canonical-flow)
                 (build-flow-in-scope prj mods scope-name scope flow-name)
                 (uprj/set-env prj (k/build-flow-in-scope-keypath scope-name flow-name))))
         proj)))

(s/fdef build-flows-in-scope
        :args (s/cat :prj        ::uprj/project
                     :mods       (s/map-of keyword? ::ucore/module)
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
                 mods  (scope-modules prj scope scope-ent cfg scope-name)]
      (uprj/set-env proj (k/make-scope-keypath scope-name) :success)
      {:prj       prj
       :mods      mods
       :scope     scope
       :scope-cfg cfg})
    (futil/recover-with 
      (fn [v]
        {:prj   (uprj/set-env proj (k/make-scope-keypath scope-name) v)
         :mods  {}
         :scope nil
         :cfg   nil}))))

(defn- build-scope
  "Builds a scope"
  [prj mods scope-name scope-cfg scope]
  (->
    (futil/mlet e/context
                [built-prj (build-flows-in-scope prj mods scope-name scope)]
                (uprj/set-env
                  built-prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    built-prj
                    (scope-mod-descs mods))))
    (futil/recover-with #(uprj/set-env prj (k/scope-deps-keypath scope-name) %))))
