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

(t/deftest module-for-invocation
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [scope (reify scopes/Scope
                (-stop [_ _] (e/right nil))
                (-write-dependencies [_ _ _ _] (e/right nil))
                (-get-config [_ _ _] (e/right nil))
                (-get-module-config [_ _ _ _ _] (e/right nil))
                (-get-implicit-modules [_ _ _] (e/right nil))
                (-resolve-name [_ _ _ name-parts] (e/right (clojure.string/join "_" name-parts))))
        mod {:ulvm.core/mod-combinator-name :nop,
             :ulvm.core/mod-descriptor {},
             :ulvm.core/config {}}
        mods {:scope {:mod mod}}
        scope-name :scope
        prj        (uprj/init {} {})
        scope-cfg  {}
        inv        (s/conform ::ucore/flow-invocation '((:mod scope) {:arg1 x}))
        m-for-inv (@#'c/module-for-invocation prj mods scope-name scope scope-cfg inv)]
    (t/is (= scope-name (:scope m-for-inv)))
    (t/is (= mod (:mod m-for-inv)))))

(t/deftest ordered-invocations
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [oi (@#'c/ordered-invocations {} {} #{})]
    (t/is (e/right? oi))
    (m/fmap
      #(t/is (= [] %))
      oi)))

(t/deftest invocation-deps
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [orig-inv '((:test-mod test-scope) {:a dep-a :b dep-b} :after [added-dep])
        inv      (s/conform ::ucore/flow-invocation orig-inv)
        deps     (@#'c/invocation-deps inv)]
    (t/is (= #{'dep-a 'dep-b 'added-dep} deps))))
