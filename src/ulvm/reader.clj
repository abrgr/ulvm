(ns ^{:author "Adam Berger"} ulvm.reader
  "ULVM Reader"
  (:require [ulvm.core]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(declare read-ulvm-dir
         read-ulvm-file
         combine-ulvm-maps
         eval-ulvm-seq
         ulvm-entity
         edn-seq)

(defn read-ulvm-dir
  "Reads a directory containing .ulvm files and returns a map of scopes, loaders, and flows."
  [directory]
  (->> directory
       (io/as-file)
       (file-seq)
       (filter #(and (.isFile %) (.endsWith (.getCanonicalPath %) ".ulvm")))
       (map read-ulvm-file)
       (combine-ulvm-maps)))

(defn read-ulvm-file
  "Reads a ulvm file and returns a map of scopes, loaders, and flows."
  [file]
  (with-open [stream (java.io.PushbackReader. (io/reader file))]
    (eval-ulvm-seq (edn-seq stream))))

(defn- combine-ulvm-maps
  "Combines maps of ulvm scopes, loaders, and flows"
  [maps]
  (apply merge-with
         (fn [old new] (conj old new))
         maps))

(defn eval-ulvm-seq
  "Evaluates a seq of ulvm forms and returns a map of scopes, loaders, and flows."
  [forms]
  (combine-ulvm-maps (map ulvm-entity forms)))

(defn- ulvm-entity
  "Transform a ulvm form into a ulvm entity"
  [form]
  (let [entity-by-name (eval form)
        entity-type (:ulvm.core/type (meta (first (vals entity-by-name))))]
    {entity-type entity-by-name}))

(defn with-file-stream
  "Invoke f with a file stream created from path-parts"
  [path-parts f]
  (let [file (apply io/file path-parts)]
    (when (.exists file)
      (some->> io/reader
               java.io.PushbackReader.
               f))))
       
(defn edn-seq
  "Reads all forms in an edn stream, returning a lazy seq"
  ([stream]
    (edn-seq stream 0))
  ([stream form-count]
    (lazy-seq (try
                (if-let [form (edn/read {:eof nil} stream)]
                  (cons form (edn-seq stream (inc form-count))))
                (catch RuntimeException e
                  ; TODO: get a real reader that provides sensible errors
                  (do (println "Error reading stream at form:" form-count)
                      (throw e)))))))
