(ns ^{:author "Adam Berger"} ulvm.runnable-envs
  "ULVM runnable-env definition"
  (:require [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [clojure.spec :as s]
            [cats.core :as m]
            [cats.monad.either :as e]))

(defn launch
  "Launches a runnable environment, returning an updated environment map"
  [re-rep env]
  (e/left nil))

(s/fdef launch
        :args (s/cat :re-rep ::ucore/runnable-env)
        :ret (su/either-of? su/any map?))

(defn stop
  "Stops a runnable environment"
  [re-rep env]
  (e/left nil))

(s/fdef stop
        :args (s/cat :re-rep ::ucore/runnable-env)
        :ret (su/either-of? su/any nil?))

(defn invoke-flow
  "Invokes the named flow with the given parameters"
  [re-rep flow-name params env]
  (e/left nil))

(s/fdef invoke-flow
        :args (s/cat :re-rep ::ucore/runnable-env
                     :flow-name keyword?
                     :params map?
                     :env map?)
        :ret (su/either-of? su/any map?))

(defn invoke-ideal-flow
  "Invokes the flow that matches the named ideal flow with the given parameters"
  [re-rep ideal-flow-name params env]
  (e/left nil))

(s/fdef invoke-ideal-flow
        :args (s/cat :re-rep ::ucore/runnable-env
                     :ideal-flow-name keyword?
                     :params map?
                     :env map?)
        :ret (su/either-of? su/any map?))
