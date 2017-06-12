(ns ^{:author "Adam Berger"} ulvm.core
  "Core types used in defining a ulvm system"
  (:require [clojure.spec :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::type
  #{::flows
    ::scopes
    ::mod-loaders
    ::runnable-envs
    ::runnable-env-loaders
    ::runners})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Flow Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::flow-invocation-module?
  (s/alt
   :scope-module (s/spec (s/cat :module-name keyword?
                                :scope symbol?))
   :local-module symbol?))

(s/def ::flow-invocations?
  (s/+
   (s/spec
    (s/cat :invocation-module ::flow-invocation-module?
           :args (s/? map?)
           :name (s/? (s/cat :as #{:as}
                             :name symbol?))))))

(s/def ::flow?
  (s/spec
   (s/cat :output-descriptor (s/map-of keyword? symbol?)
          :invocations ::flow-invocations?)))

(s/def ::flows
  (s/map-of keyword? ::flow?))

(defmacro defflow
  "Define a flow"
  [& params]
  (let [the-args (s/conform (s/cat :name keyword?
                                   :description (s/? string?)
                                   :args vector?
                                   :flow (s/+ #(or true %))) params)
        name (:name the-args)
        description (or (:description the-args) (str name))
        args (:args the-args)
        flow (:flow the-args)]
    `(makeflow ~name ~description (quote ~args) (quote ~flow))))

(defn makeflow
  [name description args flow]
  {name (with-meta flow {::type ::flows
                         ::args args
                         ::description description})})

(s/fdef makeflow
        :args (s/cat
               :name keyword?
               :description (s/? string?)
               :args (s/spec (s/* keyword?))
               :body ::flow?)
        :ret ::flows
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:body args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scope Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::scope?
  (s/keys :req [::runnable-env-ref] :opt [::modules ::init ::config]))

(s/def ::scopes
  (s/map-of keyword? ::scope?))

(s/def ::config map?)

(s/def ::init ::flow-invocations?)

(defmacro defscope
  "Define a scope, which represents a build artifact, often serving as a container for code (e.g. a C++ or a NodeJS process) or an entity of the infrastructure (e.g. a cluster of machines)"
  ([name scope]
   `(makescope ~name (str ~name) ~scope))
  ([name description scope]
   `(makescope ~name ~description (quote ~scope))))

(defn makescope
  [name description scope]
  {name (with-meta scope {::type ::scopes
                          ::description description})})

(s/fdef makescope
        :args (s/cat
               :name keyword?
               :description (s/? string?)
               :scope ::scope?)
        :ret ::scopes
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:scope args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Module Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::module
  (s/keys
   :req [(or ::mod-loader-name ::builtin-mod-loader-name)
         ::mod-descriptor]
   :opt [::config]))

(s/def ::mod-loader-name keyword?)

(s/def ::builtin-mod-loader-name #{:ulvm.mod-loaders/docker-hub})

(s/def ::mod-descriptor map?)

(s/def ::modules (s/map-of keyword? ::module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loader Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::mod-loader
  (s/keys
   :req [::runnable-env-ref]))

(s/def ::mod-loaders
  (s/map-of keyword? ::mod-loader))

(defmacro defmodloader
  "Defines a new loader, which is responsible for retrieving modules from somewhere"
  ([name loader]
   `(makemodloader ~name (str ~name) ~loader))
  ([name description loader]
   `(makemodloader ~name ~description (quote ~loader))))

(defn makemodloader
  [name description loader]
  {name (with-meta loader {::type ::mod-loaders
                           ::description description})})

(s/fdef makemodloader
        :args (s/cat
               :name keyword?
               :description string?
               :loader ::mod-loader)
        :ret (s/map-of keyword? ::mod-loader)
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:loader args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runnable Environment Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::runner-name keyword?)

(s/def ::builtin-runner-name
  #{:ulvm.runners/docker-container})

(s/def ::runner-descriptor map?)

(s/def ::runner
  (s/keys
   :req [(or ::builtin-runner-name ::runner-name)
         ::runner-descriptor]))

(s/def ::runners
  (s/map-of keyword? ::runner))

(s/def ::runnable-scope
  (s/keys
   :req [::module ::runner]))

(s/def ::runnable-scopes
  (s/map-of keyword? ::runnable-scope))

(s/def ::ideal-flows (s/coll-of keyword?))

(s/def ::exported-flow
  (s/keys
   :req [::runner ::ideal-flows]))

(s/def ::exported-flows
  (s/map-of keyword? ::exported-flow))

(s/def ::runnable-env
  (s/keys
   :req [::ns ::exported-flows]
   :opt [::runnable-scopes]))

(s/def ::runnable-envs
  (s/map-of keyword? ::runnable-env))

(defmacro defrunnableenv
  "Defines a new runnable environment"
  ([name runnable-env]
   `(makerunnableenv ~name (str ~name) ~runnable-env))
  ([name description runnable-env]
   `(makerunnableenv ~name ~description (quote ~runnable-env))))

(defn makerunnableenv
  [name description runnable-env]
  {name (with-meta runnable-env {::type ::runnable-envs
                                 ::description description})})

(s/fdef makerunnableenv
        :args (s/cat
               :name keyword?
               :description string?
               :runnable-env ::runnable-env)
        :ret ::runnable-envs
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:runnable-env args))))

(s/def ::runnable-env-descriptor map?)

(s/def ::runnable-env-ref
  (s/keys
   :req [(or ::builtin-runnable-env-loader-name
             ::runnable-env-loader-name)
         ::runnable-env-descriptor]))

(s/def ::runnable-env-loader
  (s/keys
   :req [::runnable-env-ref]))

(s/def ::runnable-env-loaders
  (s/map-of keyword? ::runnable-env-loader))

(defmacro defrunnableenvloader
  "Defines a new runnable environment loader"
  ([name runnable-env-loader]
   `(makerunnableenvloader ~name (str ~name) ~runnable-env-loader))
  ([name description runnable-env-loader]
   `(makerunnableenvloader ~name ~description (quote ~runnable-env-loader))))

(defn makerunnableenvloader
  [name description runnable-env-loader]
  {name (with-meta runnable-env-loader {::type ::runnable-env-loaders
                                        ::description description})})

(s/fdef makerunnableenvloader
        :args (s/cat
               :name keyword?
               :description string?
               :runnable-env-loader ::runnable-env-loader)
        :ret ::runnable-env-loaders
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:runnable-env-loader args))))

(s/def ::runner-def
  (s/keys
   :req [::runnable-env-ref
         (or ::flow-name ::ideal-flow)]))

(defmacro defrunner
  "Defines a new runner"
  ([name runner]
   `(makerunner ~name (str ~name) ~runner))
  ([name description runner]
   `(makerunner ~name ~description (quote ~runner))))

(defn makerunner
  [name description runner]
  {name (with-meta runner {::type ::runners
                           ::description description})})

(s/fdef makerunner
        :args (s/cat
               :name keyword?
               :description string?
               :runner ::runner-def)
        :ret ::runners
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:runner args))))
