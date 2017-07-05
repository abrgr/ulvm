(ns ^{:author "Adam Berger"} ulvm.runners
  "ULVM Runners"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]))

(defmethod uprj/run :default
  [prj ctx runner]
  nil)

; Load builtin runners
(load "runners/docker-container")
