(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.reader :as uread]
            [ulvm.runnable-envs :as renv]
            [ulvm.runners]
            [ulvm.func-utils :as futil]
            [ulvm.env-keypaths :as k]
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

(defn- scope-mod-descs-by-name
  [scope-ent]
  (->> (::ucore/modules scope-ent)
       (map
        (fn [[k v]]
          [k (::ucore/mod-descriptor v)]))
       (into {})))

(defn- build-scope-with-renv
  "Builds a scope given a runnable env"
  [prj scope-name scope-ent renv]
  (uprj/set-env
    prj
    (k/scope-deps-keypath scope-name)
    (-> prj
        (renv/launch renv)
        (renv/invoke-ideal-flow
          renv
          :org.ulvm.scope/write-dependencies
          (scope-mod-descs-by-name scope-ent)))))

(defn- build-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (let [{prj :prj, renv :el} (uprj/deref-runnable-env proj scope-ent)
        built-prj                 (m/fmap #(build-scope-with-renv prj scope-name scope-ent %) renv)]
    built-prj))
