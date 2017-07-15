(ns ^{:author "Adam Berger"} ulvm.mod-combinators
  "ULVM module combinator definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.re-loaders :as rel]
            [ulvm.spec-utils :as su]
            [cats.core :as m]
            [cats.monad.either :as e]))

; TODO: actually implement this
(deftype CustomModCombinator [runnable-env]
  uprj/ModCombinator
  (-scope-with-results [this prj invocations deps consumer result] (e/right {})))

(defn- get-rel-name
  "Runnable env loader name from loader entity"
  [loader-entity]
  (let [rel (::ucore/runnable-env-ref loader-entity)]
    (or (::ucore/builtin-runnable-env-loader-name rel)
        (::ucore/runnable-env-loader-name rel))))

(defmethod uprj/make-mod-combinator :default
  [proj combinator-name combinator-entity]
  (let [{prj :prj, runnable-env :el} (uprj/deref-runnable-env proj combinator-entity)]
    (uprj/set
     prj
     :mod-combinators
     combinator-name
     (e/right (CustomModCombinator. runnable-env)))))
