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
        g (b/build-call-graph
            (uprj/init {} {})
            :test-scope
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-config [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _ _] (e/right nil)))
            :test-flow
            deps
            (utils/flip-map deps)
            {}
            {}
            ['a 'b 'c])]
    (is (e/right? g))
    (m/fmap #(is (= %
                    '[{:provides #{a}
                       :depends-on #{}
                       :body [
                         {:provides #{b}
                          :depends-on #{a}
                          :body [
                            {:provides #{c}
                             :depends-on #{a b}
                             :body []}]}]}]))
            g)))

(deftest build-call-graph-sync-test
  (st/instrument (st/instrumentable-syms ['ulvm 'b]))
  (let [deps {'b #{'a} 'c #{'b 'a} 'a #{'x}}
        g (b/build-call-graph
            (uprj/init {} {})
            :test-scope
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-config [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _ _] (e/right nil)))
            :test-flow
            deps
            (utils/flip-map deps)
            {:mod-combinator-cfgs
              {:test-scope
               {:sync
                {:attrs {::ucore/result-in-invocation-block true}}}}}
            {'a {:scope        :test-flow
                 :mod          {::ucore/mod-combinator-name :sync
                                ::ucore/mod-descriptor      {}}
                 :result-names {:*default* 'a-result}
                 :mod-name     'a
                 :inv          {}}
             'b {:scope        :test-flow
                 :mod          {::ucore/mod-combinator-name :sync
                                ::ucore/mod-descriptor      {}}
                 :result-names {:*default* 'b-result}
                 :mod-name     'b
                 :inv          {}}
             'c {:scope        :test-flow
                 :mod          {::ucore/mod-combinator-name :sync
                                ::ucore/mod-descriptor      {}}
                 :result-names {:*default* 'c-result}
                 :mod-name     'c
                 :inv          {}}
             'x {:scope        :test-flow
                 :mod          {::ucore/mod-combinator-name :no-sync
                                ::ucore/mod-descriptor      {}}
                 :result-names {:*default* 'x-result}
                 :mod-name     'c
                 :inv          {}}}
            ['x 'a 'b 'c])]
    (is (e/right? g))
    (m/fmap #(is (= %
                    '[{:provides   #{x}
                       :depends-on #{}
                       :body       [
                       {:provides   #{a}
                        :depends-on #{x}
                        :body       []}
                       {:provides   #{b}
                        :depends-on #{a}
                        :body       []}
                       {:provides   #{c}
                        :depends-on #{a b}
                        :body       []}]}]))
            g)))
