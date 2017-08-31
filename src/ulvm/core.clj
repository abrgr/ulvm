(ns ^{:author "Adam Berger"} ulvm.core
  "Core types used in defining a ulvm system"
  (:require [clojure.spec :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::type
  #{::flows
    ::scopes
    ::mod-combinators
    ::runnable-envs
    ::runnable-env-loaders
    ::runners})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runnable Environment Ref Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::runnable-env-descriptor map?)

(s/def ::runnable-env-ref
  (s/keys
   :req [(or ::builtin-runnable-env-loader-name
             ::runnable-env-loader-name)
         ::runnable-env-descriptor]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Module Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::mod-combinator-name keyword?)

(s/def ::mod-descriptor map?)

(s/def ::macro-provider ::runnable-env-ref)

(s/def ::module
  (s/keys
   :req [::mod-combinator-name
         (or ::mod-descriptor
             ::macro-provider)]
   :opt [::config
         ::transformer-modules
         ::transformers]))

(s/def ::modules (s/map-of keyword? ::module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Flow Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::flow-invocation-module?
  (s/alt
   :scope-module (s/spec (s/cat :module-name keyword?
                                :scope symbol?))
   :local-module symbol?))

(s/def ::flow-invocation
  (s/cat :invocation-module ::flow-invocation-module?
         :args  (s/? map?)
         :name  (s/? (s/cat :as #{:as}
                            :name symbol?))
         :after (s/? (s/cat :after #{:after}
                            :names (s/coll-of symbol?)))))

(s/def ::flow-invocations?
  (s/+ (s/spec ::flow-invocation)))

(s/def ::output-descriptor
   (s/map-of keyword?
             (s/coll-of (s/or :result     symbol?
                              :sub-result (s/cat :sub-value keyword?
                                                 :value     symbol?)))))
  
(s/def ::flow-initializers
  (s/map-of symbol? ::flow-invocations?))

(s/def ::transformer-modules ::modules)

(s/def ::phase
  #{:client-pre :client-post :server-pre :server-post})

(s/def ::if list?)

(s/def ::when
  (s/keys
   :req-un [::phase ::if]))

(s/def ::do
  (s/spec ::flow-invocations?))

(s/def ::transformer
  (s/keys
   :req-un [::when ::do]))

(s/def ::transformers
  (s/map-of keyword? ::transformer))

(s/def ::flow-config
  (s/keys :opt [::output-descriptor
                ::flow-initializers
                ::transformer-modules
                ::transformers]))

(s/def ::flow?
  (s/spec
   (s/cat :config      ::flow-config
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
               :args (s/spec (s/* symbol?))
               :body ::flow?)
        :ret ::flows
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:body args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scope Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::parent-scope keyword?)

(s/def ::scope?
  (s/keys :req [::runnable-env-ref]
          :opt [::parent-scope ::modules ::init ::config]))

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
; Combinator Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::mod-combinator
  (s/keys
   :req [::runnable-env-ref]))

(s/def ::mod-combinators
  (s/map-of keyword? ::mod-combinator))

(defmacro defmodcombinator
  "Defines a new module combinator, which is responsible
   for generating ASTs for module invocations."
  ([name combinator]
   `(makemodcombinator ~name (str ~name) ~combinator))
  ([name description combinator]
   `(makemodcombinator ~name ~description (quote ~combinator))))

(defn makemodcombinator
  [name description combinator]
  {name (with-meta combinator {::type ::mod-combinators
                               ::description description})})

(s/fdef makemodcombinator
        :args (s/cat
               :name keyword?
               :description string?
               :combinator ::mod-combinator)
        :ret (s/map-of keyword? ::mod-combinator)
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:combinator args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runnable Environment Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::runner-name keyword?)

(s/def ::builtin-runner-name
  #{:ulvm.runners/docker-container
    :ulvm.runners/http})

(s/def ::runner-descriptor map?)

(s/def ::runner
  (s/keys
   :req [(or ::builtin-runner-name ::runner-name)
         ::runner-descriptor]))

(s/def ::runners
  (s/map-of keyword? ::runner))

(s/def ::artifact-descriptor map?)

(s/def ::builtin-artifact-loader-name
  #{:ulvm.artifact-loaders/project-file
    :ulvm.artifact-loaders/docker-hub})

(s/def ::artifact-loader
  (s/keys
   :req [(or ::builtin-artifact-loader-name
             ::runnable-env-ref)
         ::artifact-descriptor]))

(s/def ::runnable-scope
  (s/keys
   :req [::artifact-loader
         ::runner]))

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
