(ns ^{:author "Adam Berger"} ulvm.flows
  "Flow operations"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.project :as uprj]))

(defn module-for-invocation
  "The module that the given invocation references.
   A scope-name is used to disambiguate references to
   local modules."
  [mods scope-name inv]
  (let [inv-mod     (->> [(:invocation-module inv)]
                         (into {}))
        scope-mod      (:scope-module inv-mod)
        local-mod      (:local-module inv-mod)
        ; we have to do some gymnastics to handle local and
        ; scope modules
        mod-scope-name (-> (or (:scope scope-mod)
                               scope-name)
                           keyword)
        module-name    (-> (or (:module-name scope-mod)
                               local-mod)
                           keyword)
        module         (get-in mods [mod-scope-name module-name])]
    {:mod      module
     :mod-name module-name
     :scope    mod-scope-name}))
