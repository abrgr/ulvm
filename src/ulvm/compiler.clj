(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.reader :as uread]))

(declare ulvm-compile
         build-scopes
         build-scope)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (let [ulvm-entities (uread/read-ulvm-dir directory)
        env {} ; TODO: get a real environment
        empty-project {:entities     ulvm-entities
                       :mod-loaders  {}
                       :renv-loaders {}
                       :renvs        {}
                       :env          {::ucore/project-root directory}}
        prj-with-scopes (build-scopes empty-project)]
    prj-with-scopes)) ; TODO

(defn- build-scopes
  "Builds all scopes"
  [prj]
  (reduce
   (fn [prj [scope-name scope-ent]] (build-scope prj scope-name scope-ent))
   prj
   (get-in prj [:entities ::ucore/scopes])))

(defn- build-scope
  "Builds a scope"
  [proj scope-name scope-ent]
  (println scope-ent)
  (let [{prj :prj, renv :el} (uprj/deref-runnable-env proj scope-ent)]
    prj))
