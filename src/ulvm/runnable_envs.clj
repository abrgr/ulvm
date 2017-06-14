(ns ^{:author "Adam Berger"} ulvm.runnable-envs
  "ULVM runnable-env definition"
  (:require [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [ulvm.project :as uprj]
            [clojure.spec :as s]
            [cats.core :as m]
            [cats.monad.either :as e]))

(declare launch
         stop
         invoke-flow
         invoke-ideal-flow)

(defn- set-loaded-artifact
  [prj artifact-loader-name desc artifact-info]
  (uprj/set-env prj [:artifacts {artifact-loader-name desc}] artifact-info))

(defn- get-loaded-artifact
  [prj artifact-loader-name desc]
  (uprj/get-env prj [:artifacts {artifact-loader-name desc}]))

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

(defn- get-artifact-if-needed
  [prj artifact-loader]
  (let [desc (::ucore/artifact-descriptor artifact-loader)]
    (if (contains? artifact-loader ::ucore/builtin-artifact-loader-name)
      (let [name (::ucore/builtin-artifact-loader-name artifact-loader)]
        (if (some? (get-loaded-artifact prj name desc))
            prj
            (set-loaded-artifact prj name desc
             (builtin-load-artifact prj name desc))))
      (if (some? (get-loaded-artifact prj name desc))
          prj
          (let [{renv-prj :prj, renv :el} (uprj/deref-runnable-env prj artifact-loader)
                renv-ns (::ucore/ns renv)
                desc (::ucore/artifact-descriptor artifact-loader)]
            (set-loaded-artifact renv-prj renv-ns desc
             (custom-load-artifact renv-prj renv desc)))))))

(s/fdef get-artifact-if-needed
        :args (s/cat :prj ::uprj/project
                     :artifact-loader ::ucore/artifact-loader)
        :ret ::uprj/project)

(defn- run-scope
  [prj scope-name runnable-scope]
  (println (str "running" scope-name))
  (let [artifact-loader (::ucore/artifact-loader runnable-scope)
        artifact-prj (get-artifact-if-needed prj artifact-loader)
        ; TODO: run artifact with runner
  ]
    artifact-prj))

(defn launch
  "Launches a runnable environment, returning an updated project"
  [prj re-rep]
  (println (str "re" re-rep))
  (let [scopes (::ucore/runnable-scopes re-rep)]
    (println (str "scopes" scopes))
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
  [re-rep flow-name params env]
  (e/left nil))

(s/fdef invoke-flow
        :args (s/cat :re-rep ::ucore/runnable-env
                     :flow-name keyword?
                     :params map?
                     :env map?)
        :ret (su/either-of? su/any map?))

(defn invoke-ideal-flow
  "Invokes the flow that matches the named ideal flow with the given parameters"
  [prj re-rep ideal-flow-name params]
  (e/left nil))

(s/fdef invoke-ideal-flow
        :args (s/cat :prj ::uprj/project
                     :re-rep ::ucore/runnable-env
                     :ideal-flow-name keyword?
                     :params map?)
        :ret (su/either-of? su/any map?))
