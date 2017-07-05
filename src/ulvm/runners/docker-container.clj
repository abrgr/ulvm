(ns ^{:author "Adam Berger"} ulvm.runners.docker-container
  "ULVM builtin Docker container runner"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.docker-utils :as docker]))

(defmethod uprj/run :ulvm.runners/docker-container
  [prj ctx runner]
  (docker/create-container prj (::ucore/runner-descriptor runner)))
