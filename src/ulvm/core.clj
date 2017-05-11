(ns ulvm.core
  "Core types used in defining a ulvm system"
  (:require [clojure.spec :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Flow Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::flow? list?)

(defn defflow
  "Define a flow"
  ([name args flow]
    (defflow name (str name) args flow))
  ([name description args flow]
    {name (with-meta flow {::args args,
                            ::description description})}))

(s/fdef ::defflow
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
  (s/keys :req [::module] :opt [::modules ::init]))

(s/def ::init
  (s/map-of symbol? ::flow?))

(defn defscope
  "Define a scope, which represents a build artifact, often serving as a container for code (e.g. a C++ or a NodeJS process) or an entity of the infrastructure (e.g. a cluster of machines)"
  ([name scope]
    (defscope name (str name) scope))
  ([name description scope]
    {name (with-meta scope {::description description})}))

(s/fdef ::defscope
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
  (s/keys :req [::loader-name ::module-descriptor]))

(s/def ::loader-name keyword?)

(s/def ::module-descriptor map?)

(s/def ::modules (s/map-of keyword? ::module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loader Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::loader
  (s/keys :req [::install-deps-command ::write-deps-command ::install-loader-command]))

(s/def ::command? (s/+ symbol?))

(s/def ::command ::command?)

(s/def ::platform keyword?)

(s/def ::platform-independent-command? 
  (s/alt 
    :command-for-all-platforms ::command?
    :platform-dependent-commands (s/+ ::platform-dependent-command?)))

(s/def ::platform-dependent-command?
  (s/keys :req [::platform ::command]))

(s/def ::install-deps-command ::platform-independent-command?)
(s/def ::write-deps-command ::platform-independent-command?)
(s/def ::install-loader-command ::platform-independent-command?)

(defn defloader
  "Defines a new loader, which is responsible for retrieving modules from somewhere"
  ([name loader]
    (defloader name (str name) loader))
  ([name description loader]
    {name (with-meta loader {::description description})}))

(s/fdef ::defloader
  :args (s/cat
    :name :keyword?
    :description (s/? :string?)
    :loader ::loader)
  :ret (s/map-of :keyword? ::loader)
  :fn (fn [{args :args ret :ret}]
    (= ((:name args) ret) (:loader args))))
