(ns ^{:author "Adam Berger"} ulvm.artifact-loaders.docker-hub
  "ULVM builtin Docker Hub loader"
  (:require [ulvm.runnable-envs :as renv]
            [ulvm.docker-utils :as docker]))

(declare pull-image)

(defmethod renv/builtin-load-artifact :ulvm.artifact-loaders/docker-hub
  [prj _ desc]
  (docker/pull-image prj (:image desc)))
