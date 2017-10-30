(ns ^{:author "Adam Berger"} ulvm.call-graph-transforms.de-nest-test
  "Tests for ulvm.call-graph-transforms.de-nest"
  (:require [clojure.spec.test :as st]
            [ulvm.core :as ucore]
            [ulvm.call-graph-transforms.de-nest :as d])
  (:use     [clojure.test :only [is deftest]]))

(deftest simple-de-nest
  (st/instrument (st/instrumentable-syms 'd))
  (is
    (= [{:provides ['o]
         :depends-on []
         :body [{:provides ['a]
                 :depends-on []
                 :body []}
                {:provides ['b]
                 :depends-on ['a]
                 :body []}
                {:provides ['x]
                 :depends-on ['a]
                 :body []}]}]
       (d/transform
         {}
         {:mod-combinator-cfgs
           {:sync {:attrs {::ucore/result-in-invocation-block true}}}}
         {'a {:scope :test-scope
              :mod   {::ucore/mod-combinator-name :sync
                      ::ucore/mod-descriptor      {}}
              :inv   {}}
          'b {:scope :test-scope
              :mod   {::ucore/mod-combinator-name :sync
                      ::ucore/mod-descriptor      {}}
              :inv   {}}
          'o {:scope :test-scope
              :mod   {::ucore/mod-combinator-name :non-sync
                      ::ucore/mod-descriptor      {}}
              :inv   {}}
          'x {:scope :test-scope
              :mod   {::ucore/mod-combinator-name :sync
                      ::ucore/mod-descriptor      {}}
              :inv   {}}}
         [{:provides ['o]
           :depends-on []
           :body [{:provides ['a]
                   :depends-on []
                   :body [{:provides ['b]
                           :depends-on ['a]
                           :body [{:provides ['x]
                                   :depends-on ['a]
                                   :body []}]}]}]}]))))
