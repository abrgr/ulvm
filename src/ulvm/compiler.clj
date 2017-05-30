(ns ulvm.compiler
  "Compiler pipeline"
  (:require ulvm.reader
            ulvm.loaders))

(declare ulvm-compile)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (let [ulvm-entities (ulvm.reader/read-ulvm-dir directory)
        env {} ; TODO: get a real environment
        loaders (ulvm.loaders/resolve-loaders (:ulvm.core/loader ulvm-entities) env)]
    ulvm-entities))
