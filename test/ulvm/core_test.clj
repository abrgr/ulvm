(ns ulvm.core-test
  "Tests for ulvm.core"
  (:require [clojure.spec.test :as st]
            [ulvm.core :as ulvm])
  (:use     [clojure.test :only [deftest]]))

(defn examples []
  (ulvm/defloader :mvn
    "Description of mvn loader"
    {:ulvm.core/builtin-loader-name :docker-hub
     :ulvm.core/module-descriptor {:name "mvn"
                                   :version "1.0"}})

  (ulvm/defscope :my-scope
    "description of scope"
    {:ulvm.core/module
     {:ulvm.core/loader-name :npm
      :ulvm.core/module-descriptor {:name "my-module"}}
     :ulvm.core/modules {:adder {:ulvm.core/loader-name :mvn
                                 :ulvm.core/module-descriptor {:name "my-adder"}}
                         :db-saver {:ulvm.core/loader-name :mvn
                                    :ulvm.core/module-descriptor {:name "my-db-saver"}}}
     :ulvm.core/init ((adder {:v1 42} :as my-adder))})

  (ulvm/defflow :simple []
    ; this flow's result has g as its error value and c as its default return value
    {:err g, :ret c}
    ; invoke module A in scope-1 with no args, result is named 'a
    ((:A scope-1) {} :as a)
    ; invoke module B in scope-2 with arg-1 set to the default return of a, result is named 'b
    ((:B scope-2) {:arg-1 a} :as b)
    ; invoke module C in scope-1 with val set to the default return of a, result is named 'c
    ((:C scope-1) {:val a} :as c)
    ; invoke module G in scope-1 with first-val set to the other-ret return of a
    ((:G scope-1) {:first-val (:other-ret a)})
    ; invoke module D in scope-2 with v1 set to the default return of b and v2 set to the default return of c
    ((:D scope-2) {:v1 b, :v2 c}))

  (ulvm/defflow :err-handling []
    ; this flow's result has g as its error value and c as its default return value
    {:err g,
     :ret c}
    ; invoke module A in scope-1 with no args, result is named 'a
    ((:A scope-1) {} :as a)
    ((:match-result scope-1) {:*default* :result-a (((:B scope-1) {:the-val result-a} :as v1)
                                                    ((:C scope-1) {:val-1 result-a, :val-2 v1} :as c))
                              :*err* :err-a (((:log-err scope-1) {:err err-a}) :as g)}))

  (ulvm/defflow :err-recovery []
    ; this flow's result has c as its default return value
    {:ret c}
    ; invoke module A in scope-1 with no args, result is named 'a
    ((:A scope-1) {} :as a)
    ((:recover-from scope-1) {:result a
                              :err-1 (((:log-err scope-1) {:err err-1})
                                      ((:B scope-1) {} :as a))})))

(deftest examples-match-specs
  (st/instrument (st/instrumentable-syms 'ulvm))
  (examples))
