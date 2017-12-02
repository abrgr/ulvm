(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-deps
  "Synchronous Javascript Module Combinator Dependency Writer"
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [amazonica.aws.s3 :as s3]))

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
                   ; s3 keys do not start with a slash
        k          (string/replace-first path #"^[/]" "")
        contents   (->> (pkg-json cfg descs)
                        json/write-str)
        stream     (java.io.ByteArrayInputStream. (.getBytes contents "utf-8"))
        bucket     (System/getenv "SCOPE_NAME")]
    (s3/put-object
      {:access-key  bucket
       :secret-key  (System/getenv "SECRET_KEY")
       :endpoint    (System/getenv "FS_BASE_URI")}
      :key          k
      :bucket-name  bucket
      :input-stream stream)
    {:res path}))
