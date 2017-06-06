(ns ^{:author "Adam Berger"} ulvm.compiler
  "Compiler pipeline"
  (:require [ulvm.core :as ucore]
            [ulvm.reader :as uread]))

(declare ulvm-compile)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (let [ulvm-entities (uread/read-ulvm-dir directory)
        env {} ; TODO: get a real environment
        empty-project {:entities     ulvm-entities
                       :mod-loaders  {}
                       :renv-loaders {}
                       :renvs        {}
                       :env          {}}]
    nil)) ; TODO
