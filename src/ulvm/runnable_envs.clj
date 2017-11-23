(ns ^{:author "Adam Berger"} ulvm.runnable-envs
  "ULVM runnable-env definition"
  (:require [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [ulvm.project :as uprj]
            [ulvm.func-utils :as futil]
            [ulvm.env-keypaths :as k]
            [clojure.spec :as s]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare launch
         stop
         invoke-flow
         invoke-ideal-flow)

(defn- set-loaded-artifact
  [prj artifact-loader extra-ctxs artifact-info]
  (uprj/set-env prj (k/artifact-env-keypath artifact-loader extra-ctxs) artifact-info))

(defn- get-loaded-artifact
  [prj extra-ctxs artifact-loader]
  (uprj/get-env prj (k/artifact-env-keypath artifact-loader extra-ctxs)))

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
  [prj extra-ctxs artifact-loader]
  (let [desc (::ucore/artifact-descriptor artifact-loader)]
    (if (contains? artifact-loader ::ucore/builtin-artifact-loader-name)
      (let [name (::ucore/builtin-artifact-loader-name artifact-loader)]
        (set-loaded-artifact prj artifact-loader extra-ctxs
                             (builtin-load-artifact prj name desc)))
      (let [{renv-prj :prj, renv :el} (uprj/deref-runnable-env prj artifact-loader)
            desc (::ucore/artifact-descriptor artifact-loader)]
        (set-loaded-artifact renv-prj artifact-loader extra-ctxs
                             (custom-load-artifact renv-prj renv desc))))))

(s/fdef get-artifact
        :args (s/cat :prj             ::uprj/project
                     :extra-ctxs      (s/coll-of sequential?)
                     :artifact-loader ::ucore/artifact-loader)
        :ret ::uprj/project)

(defn- get-artifact-if-needed
  [prj extra-ctxs artifact-loader]
  (e/branch (get-loaded-artifact prj extra-ctxs artifact-loader)
            (fn [_] prj)
            (fn [artifact]
              (if (some? artifact)
                prj
                (get-artifact prj extra-ctxs artifact-loader)))))

(s/fdef get-artifact-if-needed
        :args (s/cat :prj ::uprj/project
                     :extra-ctxs      (s/coll-of sequential?)
                     :artifact-loader ::ucore/artifact-loader)
        :ret ::uprj/project)

(defn- run-artifact
  [prj extra-ctxs keypath artifact-loader runner]
  (uprj/set-env prj keypath
                (futil/mlet e/context
                            [ldr-keypath    (k/artifact-env-keypath artifact-loader extra-ctxs)
                            resolved-runner (uprj/eval-in-ctx
                                               prj
                                               (concat [ldr-keypath] extra-ctxs)
                                               runner)
                             run-result      (uprj/run prj artifact-loader resolved-runner)]
                            (e/right run-result))))

(defn- run-scope
  [prj extra-ctxs scope-name runnable-scope]
  (let [artifact-loader (::ucore/artifact-loader runnable-scope)
        runner          (::ucore/runner runnable-scope)
        artifact-prj    (get-artifact-if-needed prj extra-ctxs artifact-loader)]
    (run-artifact artifact-prj extra-ctxs (k/run-scope-keypath scope-name) artifact-loader runner)))

(defn launch
  "Launches a runnable environment, returning an updated project"
  ([prj re-rep]
    (launch prj re-rep []))
  ([prj re-rep extra-ctxs]
    (let [scopes (::ucore/runnable-scopes re-rep)]
      (reduce
       (fn [p [scope-name scope]]
         (run-scope p extra-ctxs scope-name scope))
       prj
       scopes))))

(s/fdef launch
        :args (s/cat :prj        ::uprj/project
                     :re-rep     ::ucore/runnable-env
                     :extra-ctxs (s/? (s/coll-of sequential?)))
        :ret ::uprj/project)

(defn stop
  "Stops a runnable environment"
  [prj re-rep]
  (e/left nil))

(s/fdef stop
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env)
        :ret ::uprj/project)

(defn invoke-flow
  "Invokes the named flow with the given parameters"
  [prj re-rep flow-name params]
  (futil/mlet e/context
              [runner          (get-in re-rep [::ucore/exported-flows flow-name ::ucore/runner])
               ctx             [(k/run-scope-keypath)]
               resolved-runner (uprj/eval-in-ctx prj ctx {'*params* (list 'quote params)} runner)
               run-result      @(uprj/run prj ctx resolved-runner)]
    (e/right run-result)))

(s/fdef invoke-flow
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env
                     :flow-name keyword?
                     :params map?)
        :ret (su/either-of? su/any map?))

(defn- find-ideal-flow
  [re-rep ideal-flow-name]
  (->
    (some->
      (filter
        #(contains? (::ucore/ideal-flows (val %)) ideal-flow-name)
        (::ucore/exported-flows re-rep))
      (first)
      (key))
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
        :ret (su/either-of? su/any su/any))

(load "artifact_loaders/docker_hub")
