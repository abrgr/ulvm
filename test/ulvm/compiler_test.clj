(ns ulvm.compiler-test
  "ulvm.compiler tests"
  (:require [ulvm.compiler :as c]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(t/deftest compiler-test
  (let [example-dir (.getCanonicalPath (io/file (io/as-file ".") "examples"))]
    (print (c/ulvm-compile example-dir))))
