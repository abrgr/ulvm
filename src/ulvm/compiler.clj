(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [ulvm.core :as ucore]
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

(defn- build-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (->
    (futil/mlet e/context
                [res   (scopes/make-scope proj scope-ent)
                 scope (:scope res)
                 prj   (:prj res)]
                (uprj/set-env
                  prj
                  (k/scope-deps-keypath scope-name)
                  (scopes/write-dependencies
                    scope
                    prj
                    (scope-mod-descs scope-ent)))) ; TODO: add implicit modules
    (futil/with-fallback proj)))
