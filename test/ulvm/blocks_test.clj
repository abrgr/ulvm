(ns ^{:author "Adam Berger"} ulvm.blocks-test
  "Blocks tests"
  (:require [clojure.spec.test :as st]
            [ulvm.blocks :as b]
            [ulvm.utils :as utils]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.scopes :as scopes]
            [cats.core :as m]
            [cats.monad.either :as e])
  (:use     [clojure.test :only [is deftest]]))

(deftest get-unique-deps-test
  (st/instrument (st/instrumentable-syms ['ulvm 'b]))
  (let [deps {"b" #{"a"} "c" #{"a" "b"}}]
    (is
      (= (@#'b/get-unique-deps
           deps
           (utils/flip-map deps) 
           ["b" "a"]
           "c")
         #{"b"}))))

(deftest build-call-graph-test
  (st/instrument (st/instrumentable-syms ['ulvm 'b]))
  (let [deps {'b #{'a} 'c #{'b 'a}}
        inv-a {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :js-sync
                              ::ucore/mod-descriptor       {}}
               :inv          {:invocation-module
                              [:scope-module {:module-name :a
                                              :scope 'test-scope}]
                              :args {}
                              :name {:_ :as, :name 'a}}
               :result-names {}
               :arg-names    {}
               :mod-name     'A}
        inv-b {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :js-sync
                              ::ucore/mod-descriptor       {}}
               :inv         {:invocation-module
                             [:scope-module {:module-name :b
                                             :scope 'test-scope}]
                             :args {:username
                                    [:ref [:ulvm.core/default-ref-arg 'a]]}
                             :name {:_ :as, :name 'a}}
               :result-names {}
               :arg-names    {}
               :mod-name     'A}
        inv-c {:scope        :test-scope
               :mod          {::ucore/mod-combinator-name :js-sync
                              ::ucore/mod-descriptor       {}}
               :inv          {:invocation-module
                              [:scope-module {:module-name :c
                                              :scope 'test-scope}]
                              :args {:username
                                     [:ref [:ulvm.core/default-ref-arg 'a]]
                                     :password
                                     [:ref [:ulvm.core/default-ref-arg 'b]]}
                              :name {:_ :as, :name 'a}}
               :result-names {}
               :arg-names    {}
               :mod-name     'A}
        g (b/build-call-graph
            (uprj/init {} {})
            :test-scope
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _] (e/right nil))
              (-resolve-name [scope prj name-parts] (e/right nil))
              (-write-flow [_ _ _ _ _ _] (e/right nil))
              (-get-name [_] 's)
              (-get-config [_] {}))
            :test-flow
            deps
            (utils/flip-map deps)
            {}
            {'a inv-a
             'b inv-b
             'c inv-c}
            ['a 'b 'c])]
    (is (e/right? g))
    (m/fmap #(is (= %
                    [{:provides #{'a}
                      :invs {'a inv-a}
                      :depends-on #{}
                      :body [
                        {:provides #{'b}
                         :invs {'b inv-b}
                         :depends-on #{'a}
                         :body [
                           {:provides #{'c}
                            :invs {'c inv-c}
                            :depends-on #{'a 'b}
                            :body []}]}]}]))
            g)))

(deftest build-call-graph-sync-test
  (st/instrument (st/instrumentable-syms ['ulvm 'b]))
  (let [deps {'b #{'a} 'c #{'b 'a} 'a #{'x}}
        named-invs 
          {'a {:scope        :test-flow
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor      {}}
               :result-names {:*default* 'a-result}
               :arg-names    {}
               :mod-name     'a
               :inv          {}}
           'b {:scope        :test-flow
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor      {}}
               :result-names {:*default* 'b-result}
               :arg-names    {}
               :mod-name     'b
               :inv          {}}
           'c {:scope        :test-flow
               :mod          {::ucore/mod-combinator-name :sync
                              ::ucore/mod-descriptor      {}}
               :result-names {:*default* 'c-result}
               :arg-names    {}
               :mod-name     'c
               :inv          {}}
           'x {:scope        :test-flow
               :mod          {::ucore/mod-combinator-name :no-sync
                              ::ucore/mod-descriptor      {}}
               :result-names {:*default* 'x-result}
               :arg-names    {}
               :mod-name     'c
               :inv          {}}}
        g (b/build-call-graph
            (uprj/init {} {})
            :test-scope
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _] (e/right nil))
              (-resolve-name [scope prj name-parts] (e/right nil))
              (-write-flow [_ _ _ _ _ _] (e/right nil))
              (-get-name [_] 's)
              (-get-config [_] {}))
            :test-flow
            deps
            (utils/flip-map deps)
            {:mod-combinator-cfgs
              {:test-scope
               {:sync
                {:attrs {::ucore/result-in-invocation-block true}}}}}
            named-invs
            ['x 'a 'b 'c])]
    (is (e/right? g))
    (m/fmap #(is (= %
                    [{:provides   #{'x}
                      :invs       {'x (get named-invs 'x)}
                      :depends-on #{}
                      :body       [
                      {:provides   #{'a}
                       :invs       {'a (get named-invs 'a)}
                       :depends-on #{'x}
                       :body       []}
                      {:provides   #{'b}
                       :invs       {'b (get named-invs 'b)}
                       :depends-on #{'a}
                       :body       []}
                      {:provides   #{'c}
                       :invs       {'c (get named-invs 'c)}
                       :depends-on #{'a 'b}
                       :body       []}]}]))
            g)))
