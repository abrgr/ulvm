(ns ^{:author "Adam Berger"} ulvm.call-graph-transforms.de-nest-test
  "Tests for ulvm.call-graph-transforms.de-nest"
  (:require [clojure.spec.test :as st]
            [ulvm.core :as ucore]
            [ulvm.call-graph-transforms.de-nest :as d])
  (:use     [clojure.test :only [is deftest]]))

(deftest simple-de-nest
  (st/instrument (st/instrumentable-syms 'd))
  (let [inv-a {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor       {}}
               :inv          {:invocation-module
                              [:scope-module {:module-name :a
                                              :scope 'test-scope}]
                              :args {:arg-1
                                     [:ref [:ulvm.core/default-ref-arg 'o]]}
                              :name {:_ :as, :name 'a}}
               :result-names {}
               :arg-names    {}
               :mod-name     'A}
        inv-b {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor       {}}
               :inv         {:invocation-module
                             [:scope-module {:module-name :b
                                             :scope 'test-scope}]
                             :args {:arg-1
                                    [:ref [:ulvm.core/default-ref-arg 'a]]}
                             :name {:_ :as, :name 'b}}
               :result-names {}
               :arg-names    {}
               :mod-name     'B}
        inv-x {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor       {}}
               :inv          {:invocation-module
                              [:scope-module {:module-name :c
                                              :scope 'test-scope}]
                              :args {:arg-1
                                     [:ref [:ulvm.core/default-ref-arg 'a]]}
                              :name {:_ :as, :name 'x}}
               :result-names {}
               :arg-names    {}
               :mod-name     'X}
        inv-o {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :not-sync
                              ::ucore/mod-descriptor       {}}
               :inv          {:invocation-module
                              [:scope-module {:module-name :o
                                              :scope 'test-scope}]
                              :args {}
                              :name {:_ :as, :name 'o}}
               :result-names {}
               :arg-names    {}
               :mod-name     'O}]
    (is
      (= [{:provides ['o]
           :invs     {'o inv-o}
           :depends-on []
           :body [{:provides ['a]
                   :invs     {'a inv-a}
                   :depends-on []
                   :body []}
                  {:provides ['b]
                   :invs     {'b inv-b}
                   :depends-on ['a]
                   :body []}
                  {:provides ['x]
                   :invs     {'x inv-x}
                   :depends-on ['a]
                   :body []}]}]
         (d/transform
           {}
           :test-scope
           {:mod-combinator-cfgs
             {:test-scope
              {:sync
               {:attrs {::ucore/result-in-invocation-block true}}}}}
           {'a inv-a
            'b inv-b
            'o inv-o
            'x inv-x}
           [{:provides ['o]
             :invs     {'o inv-o}
             :depends-on []
             :body [{:provides ['a]
                     :invs     {'a inv-a}
                     :depends-on []
                     :body [{:provides ['b]
                             :invs     {'b inv-b}
                             :depends-on ['a]
                             :body [{:provides ['x]
                                     :invs     {'x inv-x}
                                     :depends-on ['a]
                                     :body []}]}]}]}])))))
