(ns ^{:author "Adam Berger"} ulvm.compiler-test
  "ulvm.compiler tests"
  (:require [ulvm.compiler :as c]
            [ulvm.core :as ucore]
            [ulvm.scopes :as scopes]
            [ulvm.project :as uprj]
            [cats.monad.either :as e]
            [cats.core :as m]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.test :as t]))

(t/deftest compiler-test
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [example-dir (.getCanonicalPath (io/file (io/as-file ".") "examples" "toy"))
        prj (c/ulvm-compile {} example-dir)]
    (println prj)))
