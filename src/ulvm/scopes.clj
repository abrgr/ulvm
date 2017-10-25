(ns ^{:author "Adam Berger"} ulvm.scopes
  "Scopes"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.runnable-envs :as renv]
            [ulvm.func-utils :as futil]
            [ulvm.env-keypaths :as k]
            [cats.core :as m]
            [cats.monad.either :as e]))

(defprotocol Scope
  (-stop [scope prj]
    "Stops a scope")
  (-write-dependencies [scope prj mod-descriptors]
    "Writes scope dependencies given the set of module
     descriptors.  This is the time to create a
     dependency manifest and, if dependencies must
     be interrogated at compile-time, this is the
     time to download them as well.")
  (-get-config [scope prj config]
    "Applies the provided configuration to any 
     default configuration and returns the
     configuration to use.")
  (-get-module-config [scope prj mod-desc config]
    "Applies the provided configuration to any
     default configuration provided by the module
     and returns the configuration to use.")
  (-get-implicit-modules [scope prj config]
    "Returns a map of names to modules that the scope
     implicitly provides."))

(defn- scope-with-renv
  [renv]
  ; TODO: we should only use these with-fallbacks if we can't find
  ;       the ideal flow.  We don't want to swallow actual errors.
  (reify Scope
    (-stop [scope prj]
      (renv/stop prj renv))
    (-write-dependencies [scope prj mod-descriptors]
      (renv/invoke-ideal-flow
        prj
        renv
        :org.ulvm.scope/write-dependencies
        {:mod-descriptors mod-descriptors}))
    (-get-config [scope prj config]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-config
          {:config config})
        config))
    (-get-module-config [scope prj mod-desc config]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-module-config
          {:module-descriptors mod-desc
           :config             config})
        config))
    (-get-implicit-modules [scope prj config]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-implicit-modules
          {:config             config})
        {}))))

(defn stop
  [scope prj]
  (-stop scope prj))

(defn write-dependencies
  [scope prj mod-descriptors]
  (-write-dependencies scope prj mod-descriptors))

(defn get-config
  [scope prj config]
  (-get-config scope prj config))

(defn get-module-config
  [scope prj mod-desc config]
  (-get-module-config scope prj mod-desc config))

(defn get-implicit-modules
  [scope prj config]
  (-get-implicit-modules scope prj config))

(defn make-scope
  "Make a scope instance"
  [prj scope-ent]
  (let [{renv-prj :prj, renv-either :el} (uprj/deref-runnable-env prj scope-ent)]
    (m/extract
      (m/bimap
        (fn [err]
          {:prj   renv-prj
           :scope (e/left err)})
        (fn [renv]
          {:prj   (renv/launch renv-prj renv)
           :scope (e/right (scope-with-renv renv))})
        renv-either))))
