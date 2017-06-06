(ns ^{:author "Adam Berger"} ulvm.re-loaders
  "ULVM runnable env loaders definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.runnable-envs :as re]
            [ulvm.project :as uprj]
            [cats.core :as m]
            [cats.monad.either :as e]))

(defmulti make
  "Creates the runnable environment loader for an entity"
  (fn make-dispatcher
    [prj re-loader-name re-loader-entity] re-loader-name))

; TODO: actually implement this
(deftype CustomREnvLoader []
  uprj/REnvLoader
  (-get-runnable-env-rep [this prj desc] (e/right {})))

(defmethod uprj/get ::ucore/runnable-env-loader
  [prj _ name]
  (uprj/get-prj-el prj name make :renv-loaders :renv-loader ::ucore/runnable-env-loader))

(defmethod uprj/set ::ucore/runnable-env-loader
  [prj type name item]
  (uprj/set-prj-el prj type name item))

(load "re_loaders/builtin_project_file_re_loader")
