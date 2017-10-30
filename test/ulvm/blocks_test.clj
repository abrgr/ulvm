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
  (let [g (b/build-call-graph
            (uprj/init {} {})
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-config [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _ _] (e/right nil)))
            :test-flow
            {'b #{'a} 'c #{'b 'a}}
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
  (let [g (b/build-call-graph
            (uprj/init {} {})
            (reify scopes/Scope
              (-stop [_ _] (e/right nil))
              (-write-dependencies [_ _ _] (e/right nil))
              (-get-config [_ _ _] (e/right nil))
              (-get-module-config [_ _ _ _] (e/right nil))
              (-get-implicit-modules [_ _ _] (e/right nil)))
            :test-flow
            {'b #{'a} 'c #{'b 'a} 'a #{'x}}
            {:mod-combinator-cfgs
              {:sync {:attrs {::ucore/result-in-invocation-block true}}}}
            {'a {:scope :test-flow
                 :mod   {::ucore/mod-combinator-name :sync
                         ::ucore/mod-descriptor      {}}
                 :inv   {}}
             'b {:scope :test-flow
                 :mod   {::ucore/mod-combinator-name :sync
                         ::ucore/mod-descriptor      {}}
                 :inv   {}}
             'c {:scope :test-flow
                 :mod   {::ucore/mod-combinator-name :sync
                         ::ucore/mod-descriptor      {}}
                 :inv   {}}
             'x {:scope :test-flow
                 :mod   {::ucore/mod-combinator-name :no-sync
                         ::ucore/mod-descriptor      {}}
                 :inv   {}}}
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
