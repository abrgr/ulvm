(ns ^{:author "Adam Berger"} ulvm.compiler-test
  "ulvm.compiler tests"
  (:require [ulvm.compiler :as c]
            [clojure.java.io :as io]
            [clojure.spec.test :as st]
            [clojure.test :as t]))

(t/deftest compiler-test
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [example-dir (.getCanonicalPath (io/file (io/as-file ".") "examples"))]
    (print (c/ulvm-compile example-dir))))
