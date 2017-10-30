(ns ^{:author "Adam Berger"} ulvm.compiler-test
  "ulvm.compiler tests"
  (:require [ulvm.compiler :as c]
            [ulvm.core :as ucore]
            [cats.monad.either :as e]
            [cats.core :as m]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.test :as t]))

(t/deftest compiler-test
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [example-dir (.getCanonicalPath (io/file (io/as-file ".") "examples" "toy"))
        prj (c/ulvm-compile example-dir)]
    (println prj)))

(t/deftest module-for-invocation
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [mod {:ulvm.core/mod-combinator-name :nop,
             :ulvm.core/mod-descriptor {},
             :ulvm.core/config {}}
        mods {:scope {:mod mod}}
        scope-name :scope
        inv        (s/conform ::ucore/flow-invocation '((:mod scope) {:arg1 x}))
        m-for-inv (@#'c/module-for-invocation mods scope-name inv)]
    (t/is (= scope-name (:scope m-for-inv)))
    (t/is (= mod (:mod m-for-inv)))))

(t/deftest ordered-invocations
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [oi (@#'c/ordered-invocations {} {} #{})]
    (t/is (e/right? oi))
    (m/fmap
      #(t/is (= [] %))
      oi)))
