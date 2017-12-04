(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-deps
  "NodeJS Dependency Writer"
  (:require [cheshire.core :as json]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [ulvm.scopes.nodejs.write-file :as write-file]))

(defn- get-import
  [desc]
  (cond
    (contains? desc :local-filename)
    (e/right {:import (get desc :local-filename)})
    (contains? desc :npm-module)
    (e/right {:import (get  desc :npm-module)
              :version (get desc :npm-version)})
    :else
    (e/left {:err "Unknown module import type" :desc desc})))

(defn- gather-descs
  [descs]
  (reduce
    (fn [reps desc]
      (let [imp       (get-import desc)
            imp-quals (filter #(#{:prop} %) desc)]
        (-> (merge imp imp-quals)
            (conj reps))))
    #{}
    descs))

(defn- base-pkg-json
  [cfg]
  (get cfg :pkg-cfg))

(defn npm-pkgs
  [descs]
  (reduce
    (fn [deps desc]
      (if (contains? desc :npm-module)
        (merge deps {(get  desc :npm-module) (get desc :npm-version)})
        deps))
    {}
    descs))

(defn pkg-json
  [cfg descs]
  (merge (base-pkg-json cfg)
         {:dependencies (npm-pkgs descs)}))

(defn write
  [cfg descs]
  (let [path       (-> (get cfg :ulvm.scopes/gen-src-dir)
                       (io/file "package.json")
                       (.getPath))
        contents   (-> (pkg-json cfg descs)
                       (json/generate-string {:pretty true, :escape-non-ascii true}))]
    (write-file/w path contents)
    {:res path}))
