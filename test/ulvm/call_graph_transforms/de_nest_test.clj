(ns ^{:author "Adam Berger"} ulvm.call-graph-transforms.de-nest-test
  "Tests for ulvm.call-graph-transforms.de-nest"
  (:require [clojure.spec.test :as st]
            [ulvm.call-graph-transforms.de-nest :as d])
  (:use     [clojure.test :only [is deftest]]))

(deftest simple-de-nest
  (st/instrument (st/instrumentable-syms 'd))
  (is
    (= [{:provides [:o]
         :depends-on []
         :body [{:provides [:a]
                 :depends-on []
                 :body []}
                {:provides [:b]
                 :depends-on [:a]
                 :body []}
                {:provides [:x]
                 :depends-on [:a]
                 :body []}]}]
       (d/transform
         {}
         {:mod-combinator-configs
           {:sync {:attrs {:ulvm.core/result-in-invocation-block true}}}}
         {:a {:mod-combinator :sync}
          :b {:mod-combinator :sync}
          :x {:mod-combinator :sync}}
         [{:provides [:o]
           :depends-on []
           :body [{:provides [:a]
                   :depends-on []
                   :body [{:provides [:b]
                           :depends-on [:a]
                           :body [{:provides [:x]
                                   :depends-on [:a]
                                   :body []}]}]}]}]))))
