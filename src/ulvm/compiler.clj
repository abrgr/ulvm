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
            [ulvm.env-keypaths :as k]
            [ulvm.scopes :as scopes]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare ulvm-compile
         build-scopes
         build-scope)

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

(defn- build-scopes
  "Builds all scopes"
  [prj]
  (reduce
   (fn [prj [scope-name scope-ent]] (build-scope prj scope-name scope-ent))
   prj
   (get-in prj [:entities ::ucore/scopes])))

(defn- scope-mod-descs
  [scope-ent]
  (->> (::ucore/modules scope-ent)
       (map
        (fn [[k v]]
          (::ucore/mod-descriptor v)))
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
      (->
        (invocation-name inv)
        (#(conj named [% inv]))))
    {}))

(defn- ordered-invocations
  "Returns a seq of topologically ordered invocations"
  [invocations]
  (-> (named-invocations invocations)

      (identity)))

(defn- build-flow-in-scope
  "Builds the portion of a flow contained in a scope"
  [proj scope-name scope flow-ent]
  (-> (ordered-invocations (::ucore/flow-invocations flow-ent))
      (println)
      (identity)))

(defn- build-flows-in-scope
  "Builds all flows for the given scope."
  [proj scope-name scope]
  (->> (get-in proj [:entities ::ucore/flow])
       (reduce
         (fn [prj f]
           (uprj/set-env
            prj
            [:agb (gensym)]
            (->> f
                 (uprj/canonical-flow)
                 (build-flow-in-scope prj scope-name scope))))
         proj)))

(s/fdef build-flows-in-scope
        :args (s/cat :prj        ::uprj/project
                     :scope-name keyword?
                     :scope      #(satisfies? scopes/Scope %))
        :ret  ::uprj/project)

(defn- build-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (->
    (futil/mlet e/context
                [res       (scopes/make-scope proj scope-ent)
                 scope     (:scope res)
                 prj       (:prj res)
                 built-prj (build-flows-in-scope prj scope-name scope)]
                (uprj/set-env
                  built-prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    built-prj
                    (scope-mod-descs scope-ent)))) ; TODO: add implicit modules
    ; TODO: weird that we only write errors here
    (futil/recover-with #(uprj/set-env proj (k/build-scope-keypath scope-name) %))))
