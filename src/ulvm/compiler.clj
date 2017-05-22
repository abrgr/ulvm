(ns ulvm.compiler
  "Compiler pipeline"
  (:require [ulvm.reader]))

(declare ulvm-compile)

(defn ulvm-compile
  "Compile a ulvm system"
  [directory]
  (-> directory
      ulvm.reader/read-ulvm-dir))
