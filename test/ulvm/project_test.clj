(ns ^{:author "Adam Berger"} ulvm.project-test
  "Project tests"
  (:require [clojure.spec.test :as st]
            [ulvm.core :as ulvm]
            [ulvm.reader :as uread]
            [ulvm.project :as uprj]
            [ulvm.mod-loaders]
            [ulvm.re-loaders])
  (:use     [clojure.test :only [is deftest]]))

(def flowfile-example
  `[(ulvm/defmodloader :mvn
      "Description of mvn loader"
      {:ulvm.core/runnable-env-loader
       {:ulvm.core/builtin-runnable-env-loader-name :ulvm.re-loaders/project-file
        :ulvm.core/runnable-env-descriptor {:path "re-loaders/mvn.ulvm"}}})

    (ulvm/defscope :my-scope
      "Description of scope"
      {:ulvm.core/module
       {:ulvm.core/mod-loader-name :npm
        :ulvm.core/mod-descriptor {:name "my-module"}}
       :ulvm.core/modules {:adder {:ulvm.core/mod-loader-name :mvn
                                   :ulvm.core/mod-descriptor {:name "my-adder"}}
                           :db-saver {:ulvm.core/mod-loader-name :mvn
                                      :ulvm.core/mod-descriptor {:name "my-db-saver"}}}
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
                                        ((:B scope-1) {} :as a))}))])

(deftest get-mod-loader
  (st/instrument (st/instrumentable-syms ['ulvm 'uprj]))
  (let [ents (uread/eval-ulvm-seq flowfile-example)
        prj {:entities     ents
             :mod-loaders  {}
             :renv-loaders {}
             :renvs        {}
             :env          {}}]
    (uprj/get prj ::ulvm/mod-loader :mvn)))