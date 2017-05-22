(ns ulvm.core
  "Core types used in defining a ulvm system"
  (:require [clojure.spec :as s]))

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
  {name (with-meta flow {::type ::flow
                         ::args args,
                         ::description description})})

(s/fdef makeflow
        :args (s/cat
               :name keyword?
               :description (s/? string?)
               :args (s/spec (s/* keyword?))
               :body ::flow?)
        :ret (s/map-of keyword? ::flow?)
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:body args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scope Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::scope?
  (s/keys :req [::module] :opt [::modules ::init ::config]))

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
  {name (with-meta scope {::type ::scope
                          ::description description})})

(s/fdef makescope
        :args (s/cat
               :name keyword?
               :description (s/? string?)
               :scope ::scope?)
        :ret (s/map-of keyword? ::scope?)
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:scope args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Module Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::module
  (s/keys
   :req [(or ::loader-name ::builtin-loader-name)
         ::module-descriptor]
   :opt [::config]))

(s/def ::loader-name keyword?)

(s/def ::builtin-loader-name #{:docker-hub :npm})

(s/def ::module-descriptor map?)

(s/def ::modules (s/map-of keyword? ::module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loader Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defloader
  "Defines a new loader, which is responsible for retrieving modules from somewhere"
  ([name loader]
   `(makeloader ~name (str ~name) ~loader))
  ([name description loader]
   `(makeloader ~name ~description (quote ~loader))))

(defn makeloader
  [name description loader]
  {name (with-meta loader {::type ::loader
                           ::description description})})

(s/fdef makeloader
        :args (s/cat
               :name keyword?
               :description string?
               :loader ::module)
        :ret (s/map-of keyword? ::module)
        :fn (fn [{args :args ret :ret}]
              (= ((:name args) ret) (:loader args))))
