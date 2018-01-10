(ns ^{:author "Adam Berger"} ulvm.flows-test
  "ulvm.flows tests"
  (:require [ulvm.flows :as f]
            [ulvm.project :as uprj]
            [ulvm.core :as ucore]
            [cats.monad.either :as e]
            [cats.core :as m]
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

(t/deftest ordered-invocations
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [oi (f/ordered-invocations {} {} #{})]
    (t/is (e/right? oi))
    (m/fmap
      #(t/is (= [] %))
      oi)))

(t/deftest invocation-deps
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [orig-inv '((:test-mod test-scope) {:a dep-a :b dep-b} :after [added-dep])
        inv      (s/conform ::ucore/flow-invocation orig-inv)
        deps     (@#'f/invocation-deps inv)]
    (t/is (= #{'dep-a 'dep-b 'added-dep} deps))))

(t/deftest expand-transformers
  (st/instrument (st/instrumentable-syms 'ulvm))
  (let [mods             {:scope-1 {:A {::ucore/mod-combinator-name :js-sync
                                        ::ucore/mod-descriptor {:npm "a"}}}
                          :scope-2 {:B {::ucore/mod-combinator-name :js-sync
                                        ::ucore/mod-descriptor {:npm "b"}
                                        ::ucore/transformers {
                                          :send-http-req {
                                            :when {
                                              :phase :client-pre
                                              :if    '(contains? (:tags *client-scope-cfg*) :js)}
                                            :do '[
                                              (prep-data {:other arg-1
                                                          :body  (:body arg-1)} :as a)
                                              (http-send {:method :post
                                                          :path   "a/b"
                                                          :other  a
                                                          :body   (:body a)
                                                          :orig   arg-1})]}}}}}
        named-scope-cfgs {:scope-1 {:tags #{:js}}
                          :scope-2 {:tags #{:js}}}
        flows            (ucore/makeflow
                          :my-flow
                          "my-flow"
                          '[some-arg]
                          '[{::ucore/home-scope :scope-1}
                            ((:A scope-1) {:arg-1 {"k1" "v1"}} :as a)
                            ((:B scope-2) {:arg a} :as b)])
        canonical-flow   (s/conform ::ucore/flow (:my-flow flows))
        prj              (uprj/init {} {})
        expanded         (f/expand-transformers prj mods named-scope-cfgs canonical-flow)]
    (println expanded)))
