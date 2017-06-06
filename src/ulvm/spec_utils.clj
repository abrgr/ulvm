(ns ^{:author "Adam Berger"} ulvm.spec-utils
  "Spec utilities"
  (:require [cats.monad.either :as e]))

(defn any [_] true)

(defn either-of?
  "Predicate checking whether the value is an either and whether it satisfies left-fn if a left or right-fn if a right"
  [left-fn right-fn]
  #(and (e/either? %)
        (e/branch  % left-fn right-fn)))
