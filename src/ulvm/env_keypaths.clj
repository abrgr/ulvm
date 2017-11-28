(ns ^{:author "Adam Berger"} ulvm.env-keypaths
  "ULVM keypaths"
  (:require [ulvm.core :as ucore]))

(defn artifact-env-keypath
  ([]
    [:artifacts])
  ([artifact-loader]
    (let [id   (or (::ucore/builtin-artifact-loader-name artifact-loader)
                   (::ucore/runnable-env-ref artifact-loader))
          desc (::ucore/artifact-descriptor artifact-loader)]
      (conj (artifact-env-keypath) {id desc})))
  ([artifact-loader extra-ctxs]
   (conj
     (artifact-env-keypath artifact-loader)
     {:extra-ctxs extra-ctxs})))

; TODO: needs to be namespaced under the renv
(defn run-scope-keypath
  "Keypath for the result of running a scope"
  ([]
    [:runs :scope])
  ([scope-name]
    (conj (run-scope-keypath) scope-name)))

(defn scope-deps-keypath
  "Keypath for the result of writing scope dependencies"
  ([]
    [:scope-deps])
  ([scope-name]
    (conj (scope-deps-keypath) scope-name)))

(defn make-scope-keypath
  "Keypath for the result of making a scope"
  ([]
    [:make-scope])
  ([scope-name]
    (conj (make-scope-keypath) scope-name)))

(defn build-flow-in-scope-keypath
  "Keypath for the result of building a flow for a scope"
  ([]
    [:build-flow-in-scope])
  ([scope-name]
    (conj (build-flow-in-scope-keypath) scope-name))
  ([scope-name flow-name]
    (conj (build-flow-in-scope-keypath scope-name) flow-name)))

(defn scope-config-keypath
  "Keypath for the configuration of a scope"
  ([]
    [:scope-config])
  ([scope-name]
    (conj (scope-config-keypath) scope-name)))

(defn project-root
  "Project root directory on the host"
  []
  [::ucore/project-root])

(defn gen-src-root
  "Generated source root directory for a scope"
  ([]
    [::ucore/gen-src-root])
  ([scope-name]
    (conj (gen-src-root) scope-name)))

(defn build-root
  "Build artifact root directory for a scope"
  ([]
    [::ucore/build-root])
  ([scope-name]
    (conj (build-root) scope-name)))

(defn fileserver-base-uri
  "Base URI for the embedded file server that we run to allow scopes to write
   to disk"
  []
  [::ucore/fileserver-base-uri])

(defn fileserver-ip
  "IP for the embedded file server that we run to allow scopes to write
   to disk"
  []
  [::ucore/fileserver-ip])
