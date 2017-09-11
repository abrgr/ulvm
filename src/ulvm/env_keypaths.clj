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

(defn build-scope-keypath
  "Keypath for the result of building a scope"
  ([]
    [:build-scope])
  ([scope-name]
    (conj (build-scope-keypath) scope-name)))
