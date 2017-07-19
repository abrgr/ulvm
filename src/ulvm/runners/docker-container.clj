(ns ^{:author "Adam Berger"} ulvm.runners.docker-container
  "ULVM builtin Docker container runner"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.docker-utils :as docker]
            [ulvm.func-utils :as futil]
            [cats.monad.either :as e]))

(defmethod uprj/run :ulvm.runners/docker-container
  [prj ctx runner]
  (futil/mlet e/context
              [create-result (docker/create-container prj (::ucore/runner-descriptor runner))
               create-data (docker/get-container-output prj (:container-id create-result))]
              (e/right (merge create-result {:result create-data}))))
