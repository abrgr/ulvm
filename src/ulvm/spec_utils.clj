(ns ^{:author "Adam Berger"} ulvm.spec-utils
  "Spec utilities"
  (:require [clojure.spec :as s]
            [cats.monad.either :as e]))

(def any
  (s/with-gen
    (s/spec (fn [_] true))
    #(s/gen #{{:any :thing}})))

(defn either-of?
  "Predicate checking whether the value is an either and whether it satisfies left-fn if a left or right-fn if a right"
  [left-fn right-fn]
  #(and (e/either? %)
        (e/branch  % left-fn right-fn)))
