(ns ^{:author "Adam Berger"} ulvm.compiler-test
  "ulvm.flows tests"
  (:require [ulvm.flows :as f]
            [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.test :as t]))

(t/deftest module-for-invocation
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [mod {:ulvm.core/mod-combinator-name :nop,
             :ulvm.core/mod-descriptor {},
             :ulvm.core/config {}}
        mods {:scope {:mod mod}}
        scope-name :scope
        inv        (s/conform ::ucore/flow-invocation '((:mod scope) {:arg1 x}))
        m-for-inv (f/module-for-invocation mods scope-name inv)]
    (t/is (= scope-name (:scope m-for-inv)))
    (t/is (= mod (:mod m-for-inv)))))

