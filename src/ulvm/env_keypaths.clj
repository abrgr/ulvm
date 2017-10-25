(ns ^{:author "Adam Berger"} ulvm.env-keypaths
  "ULVM keypaths"
  (:require [ulvm.core :as ucore]))

(defn artifact-env-keypath
  ([artifact-loader]
    (let [id   (or (::ucore/builtin-artifact-loader-name artifact-loader)
                   (::ucore/runnable-env-ref artifact-loader))
          desc (::ucore/artifact-descriptor artifact-loader)]
      (conj (artifact-env-keypath) {id desc})))
  ([]
    [:artifacts]))

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
