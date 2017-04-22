(ns ulvm.core
  (:require [clojure.spec :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Flow Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::flow? list?)

(defn defflow
  "Define a flow"
  [name description args flow]
  ({name (with-meta flow {::args args
                          ::description description})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scope Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::scope? 
  (s/keys :req [::module] :opt [::modules ::init]))

(s/def ::init map?) ; TODO map from names to flows

(defn defscope
  "Define a scope, which represents a build artifact, often serving as a container for code (e.g. a C++ or a NodeJS process) or an entity of the infrastructure (e.g. a cluster of machines)"
  [name description scope]
  ({name (with-meta scope {::description description})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Module Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::module
  (s/keys :req [::loader-name ::module-descriptor]))

(s/def ::loader-name symbol?)

(s/def ::module-descriptor map?)

(s/def ::modules (s/+ ::module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loader Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::loader
  (s/keys :req [::install-deps-command ::write-deps-command ::install-loader-command]))

(s/def ::command? (s/+ symbol?))

(s/def ::command ::command?)

(s/def ::platform symbol?)

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
  [name description loader]
  ({name (with-meta loader {::description description})}))
