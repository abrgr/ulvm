(ns ^{:author "Adam Berger"} ulvm.core-spec-test
  "Spec tests for ulvm.core"
  (:require [clojure.spec.test :as st]
            [ulvm.core :as ulvm])
  (:use     [clojure.test :only [deftest]]))

(defn flowfile-examples []
  (ulvm/defmodloader :mvn
    "Description of mvn loader"
    {:ulvm.core/runnable-env-ref
     {:ulvm.core/builtin-runnable-env-loader-name :ulvm.runnable-env-loaders/http
      :ulvm.core/runnable-env-descriptor {:url "http://github.com/ulvm/contrib/1.2.3/mvn-runnable-env.ulvm"}}})

  (ulvm/defscope :my-scope
    "Description of scope"
    {:ulvm.core/runnable-env-ref
     {:ulvm.core/builtin-runnable-env-loader-name :ulvm.core/project-file
      :ulvm.core/runnable-env-descriptor {:path "scopes/nodejs.ulvm"}}
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
                                      ((:B scope-1) {} :as a))})))

(defn runnable-env-examples []
  (ulvm.core/defrunnableenvloader :auth-http-re-loader
    "Defines an http runnable environment loader that uses some special auth scheme"
    {:ulvm.core/runnable-env-ref
     {:ulvm.core/builtin-runnable-env-loader-name :ulvm.runnable-env-loaders/http
      :ulvm.core/runnable-env-descriptor {:url "http://github.com/ulvm/contrib/1.2.3/runnable-env-loaders.ulvm"}}})

  (ulvm.core/defrunner :http-get-runner
    "Sends a get request"
    {:ulvm.core/runnable-env-ref
     {:ulvm.core/builtin-runnable-env-loader-name :ulvm.runnable-env-loaders/http
      :ulvm.core/runnable-env-descriptor {:url "http://github.com/ulvm/contrib/1.2.3/runnable-env-loaders.ulvm"}}
     :ulvm.core/ideal-flow ulvm.core.get-runnable-env})

  (ulvm.core/defrunnableenv :mvn
    "Maven Loader Environment"
    {:ulvm.core/ns :org.ulvm.maven
     :ulvm.core/runnable-scopes
     {:mvn
      {:ulvm.core/artifact-loader
       {:ulvm.core/builtin-artifact-loader-name :ulvm.artifact-loaders/docker-hub
        :ulvm.core/artifact-descriptor {:name "ulvm-mvn"
                                        :version "1.0"}} ; module adds items to env under joint namespace ({ns}, ulvm.mod-loaders.docker-hub)/{scope-name}/ (e.g. #{org.ulvm.maven ulvm.mod-loaders.docker-hub}/mvn/{docker-image, version, etc})
       :ulvm.core/runner
       {:ulvm.core/builtin-runner-name :ulvm.runners/docker-container
        :ulvm.core/runner-descriptor
        {:volumes [{:src (ulvm.core/from-env :org.ulvm.maven/mvn-cache-dir)
                    :dest "/m2/cache"
                    :mode "ro"}]}
        :env {:mvn-username :org.ulvm.maven/mvn-username}}}}
     :ulvm.core/exported-flows
     {:org.ulvm.maven/mvn-install
      {:ulvm.core/runner
       {:ulvm.core/runner-name :ulvm.runners/http
        :ulvm.core/runner-descriptor {:method "post"
                                      :host (ulvm.core/from-env [#{:org.ulvm.maven :ulvm.runners.docker-container} :mvn :container-ip])
                                      :body :ulvm.core/mod-descriptor}}
       :ulvm.core/ideal-flows #{:org.ulvm.mod-loader}}}}))

(deftest flowfile-examples-match-specs
  (st/instrument (st/instrumentable-syms 'ulvm))
  (flowfile-examples))

; TODO: a custom loader specifies a runnable-env loader; we pull the env-template, run all of the scopes (do we need ordering among them?), find the org.ulvm.loader ideal-flow, and invoke it.
; TODO: we build up an environment map with the results returned from running all scopes and make that env available when running a flow runner
(deftest runnable-env-examples-match-specs
  (st/instrument (st/instrumentable-syms 'ulvm))
  (runnable-env-examples))
