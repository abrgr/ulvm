(ns ^{:author "Adam Berger"} ulvm.mod-combinators
  "ULVM module combinator definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.re-loaders :as rel]
            [ulvm.runnable-envs :as renv]
            [ulvm.func-utils :as futil]
            [ulvm.spec-utils :as su]
            [cats.core :as m]
            [cats.monad.either :as e]))

(deftype CustomModCombinator [renv]
  uprj/ModCombinator
  (-block-with-results [this prj config invocations body]
    (m/->>= (e/right {:invocations invocations
                      :config      config
                      :body        body})
            (renv/invoke-ideal-flow
              prj
              renv
              :org.ulvm.mod-combinator/block-with-results)
            ((comp m/return first))))
  (-get-mod-combinator-config [this prj config]
    ; TODO: with-fallback is silly here; don't swallow actual errors
    (futil/with-fallback
      (m/->>= (e/right {:config config})
              (renv/invoke-ideal-flow
                prj
                renv
                :org.ulvm.mod-combinator/get-config)
              ((comp m/return first)))
      config)))

(defn- get-rel-name
  "Runnable env loader name from loader entity"
  [loader-entity]
  (let [rel (::ucore/runnable-env-ref loader-entity)]
    (or (::ucore/builtin-runnable-env-loader-name rel)
        (::ucore/runnable-env-loader-name rel))))

(defmethod uprj/make-mod-combinator :default
  [proj combinator-name combinator-entity]
  (futil/mlet e/context
              [p-el   (uprj/deref-runnable-env proj combinator-entity)
               re-rep (:el p-el)
               prj    (renv/launch (:prj p-el) re-rep)]
    (uprj/set
     prj
     :mod-combinators
     combinator-name
     (e/right (CustomModCombinator. re-rep)))))
