(ns ^{:author "Adam Berger"} ulvm.utils-test
  "Spec tests for ulvm.utils"
  (:require [clojure.spec.test :as st]
            [ulvm.utils :as u])
  (:use     [clojure.test :only [is deftest]]))

(deftest topo-sort-sat
  (st/instrument (st/instrumentable-syms 'u))
  (let [sorted (u/topo-sort {:a [:b :c], :b [:c]} [:a :d :c :b])]
    (is (empty? (:unsat sorted)))
    (is (= #{:a :b :c :d} (:visited sorted)))
    (is (= [:d :c :b :a] (:items sorted)))))

(deftest topo-sort-unsat
  (st/instrument (st/instrumentable-syms 'u))
  (let [sorted (u/topo-sort {:a [:b :c], :b [:a]} [:a :d :c :b])]
    (is (= #{:b} (:unsat sorted)))
    (is (= #{:a :b :c :d} (:visited sorted)))
    (is (= [:d :c :b :a] (:items sorted)))))

(deftest topo-sort-empty
  (st/instrument (st/instrumentable-syms 'u))
  (let [sorted (u/topo-sort {} [])]
    (is (= #{} (:unsat sorted)))
    (is (= #{} (:visited sorted)))
    (is (= [] (:items sorted)))))

(deftest flip-map-test
  (st/instrument (st/instrumentable-syms 'ulvm))
  (is (= (u/flip-map {:a [:b :c], :b [:c :d]})
         {:b #{:a}, :c #{:a, :b}, :d #{:b}})))

(deftest get-in
  (st/instrument (st/instrumentable-syms 'ulvm))
  (is
    (=
     (u/get-in
       [:ref [:ulvm.core/named-ref-arg
              {:sub-result :username
               :result 'authorized-login}]]
       [:ref :ulvm.core/named-ref-arg :sub-result])
     :username)))
