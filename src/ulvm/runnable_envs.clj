(ns ^{:author "Adam Berger"} ulvm.runnable-envs
  "ULVM runnable-env definition"
  (:require [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [ulvm.project :as uprj]
            [ulvm.func-utils :as futil]
            [clojure.spec :as s]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare launch
         stop
         invoke-flow
         invoke-ideal-flow)

(defn- artifact-env-keypath
  [artifact-loader]
  (let [id   (or (::ucore/builtin-artifact-loader-name artifact-loader)
                 (::ucore/runnable-env-ref artifact-loader))
        desc (::ucore/artifact-descriptor artifact-loader)]
    [:artifacts {id desc}]))

(defn- set-loaded-artifact
  [prj artifact-loader artifact-info]
  (uprj/set-env prj (artifact-env-keypath artifact-loader) artifact-info))

(defn- get-loaded-artifact
  [prj artifact-loader]
  (uprj/get-env prj (artifact-env-keypath artifact-loader)))

(defmulti builtin-load-artifact
  "Retrieve an artifact with a builtin loader"
  (fn builtin-load-artifact
    [prj builtin-name desc]
    builtin-name))

(s/fdef builtin-load-artifact
        :args (s/cat :prj ::uprj/project
                     :builtin-name ::ucore/builtin-artifact-loader-name
                     :desc ::ucore/artifact-descriptor)
        :ret ::uprj/project)

(defmethod builtin-load-artifact :default
  [prj builtin-name desc]
  (e/left (str builtin-name " is not a builtin artifact loader")))

(defn- custom-load-artifact
  "Retrieve an artifact with a custom loader"
  [prj renv desc]
  (let [launched-prj (launch prj renv)
        params {:artifact-descriptor desc}]
    (invoke-ideal-flow prj renv :org.ulvm.load-artifact params)))

(s/fdef custom-load-artifact
        :args (s/cat :prj ::uprj/project
                     :renv ::ucore/runnable-env
                     :desc ::ucore/artifact-descriptor)
        :ret ::uprj/project)

(defn- get-artifact
  [prj artifact-loader]
  (let [desc (::ucore/artifact-descriptor artifact-loader)]
    (if (contains? artifact-loader ::ucore/builtin-artifact-loader-name)
      (let [name (::ucore/builtin-artifact-loader-name artifact-loader)]
        (set-loaded-artifact prj artifact-loader
                             (builtin-load-artifact prj name desc)))
      (let [{renv-prj :prj, renv :el} (uprj/deref-runnable-env prj artifact-loader)
            desc (::ucore/artifact-descriptor artifact-loader)]
        (set-loaded-artifact renv-prj artifact-loader
                             (custom-load-artifact renv-prj renv desc))))))

(s/fdef get-artifact
        :args (s/cat :prj ::uprj/project
                     :artifact-loader ::ucore/artifact-loader)
        :ret ::uprj/project)

(defn- get-artifact-if-needed
  [prj artifact-loader]
  (e/branch (get-loaded-artifact prj artifact-loader)
            (fn [_] prj)
            (fn [artifact]
              (if (some? artifact)
                prj
                (get-artifact prj artifact-loader)))))

(s/fdef get-artifact-if-needed
        :args (s/cat :prj ::uprj/project
                     :artifact-loader ::ucore/artifact-loader)
        :ret ::uprj/project)

(defn- run-scope-keypath
  ([]
    [:runs :scope])
  ([scope-name]
    (conj (run-scope-keypath) scope-name)))

(defn- run-artifact
  [prj keypath artifact-loader runner]
  (uprj/set-env prj keypath
                (futil/mlet e/context
                            [resolved-runner (uprj/eval-in-ctx prj (artifact-env-keypath artifact-loader) runner)
                             run-result      (uprj/run prj artifact-loader resolved-runner)]
                            (e/right run-result))))

(defn- run-scope
  [prj scope-name runnable-scope]
  (let [artifact-loader (::ucore/artifact-loader runnable-scope)
        runner          (::ucore/runner runnable-scope)
        artifact-prj    (get-artifact-if-needed prj artifact-loader)]
    (run-artifact artifact-prj (run-scope-keypath scope-name) artifact-loader runner)))

(defn launch
  "Launches a runnable environment, returning an updated project"
  [prj re-rep]
  (let [scopes (::ucore/runnable-scopes re-rep)]
    (reduce
     (fn [p [scope-name scope]]
       (run-scope p scope-name scope))
     prj
     scopes)))

(s/fdef launch
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env)
        :ret ::uprj/project)

(defn stop
  "Stops a runnable environment"
  [re-rep env]
  (e/left nil))

(s/fdef stop
        :args (s/cat :re-rep ::ucore/runnable-env)
        :ret (su/either-of? su/any map?))

(defn invoke-flow
  "Invokes the named flow with the given parameters"
  [prj re-rep flow-name params]
  (futil/mlet e/context
              [runner          (get-in re-rep [::ucore/exported-flows flow-name ::ucore/runner])
               ctx             (run-scope-keypath)
               resolved-runner (uprj/eval-in-ctx prj ctx {'*params* params} runner)
               run-result      @(uprj/run prj ctx resolved-runner)]
    prj))

(s/fdef invoke-flow
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env
                     :flow-name keyword?
                     :params map?)
        :ret (su/either-of? su/any map?))

(defn- find-ideal-flow
  [re-rep ideal-flow-name]
  (some->
    (filter
      #(contains? (::ucore/ideal-flows (val %)) ideal-flow-name)
      (::ucore/exported-flows re-rep))
    (first)
    (key)
    (futil/opt-either (str "No ideal flow [" ideal-flow-name "] in " (::ucore/ns re-rep)))))

(defn invoke-ideal-flow
  "Invokes the flow that matches the named ideal flow with the given parameters"
  [prj re-rep ideal-flow-name params]
  (futil/mlet e/context
              [flow       (find-ideal-flow re-rep ideal-flow-name)]
              (invoke-flow prj re-rep flow params)))

(s/fdef invoke-ideal-flow
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env
                     :ideal-flow-name keyword?
                     :params map?)
        :ret ::uprj/project)

(load "artifact_loaders/docker_hub")
