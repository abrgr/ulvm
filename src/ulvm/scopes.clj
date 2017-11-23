(ns ^{:author "Adam Berger"} ulvm.scopes
  "Scopes"
  (:require [ulvm.core :as ucore]
            [ulvm.project :as uprj]
            [ulvm.runnable-envs :as renv]
            [ulvm.func-utils :as futil]
            [ulvm.utils :as u]
            [ulvm.env-keypaths :as k]
            [cats.core :as m]
            [cats.monad.either :as e]))

(defprotocol Scope
  (-stop [scope prj]
    "Stops a scope")
  (-write-dependencies [scope prj cfg mod-descriptors]
    "Writes scope dependencies given the set of module
     descriptors.  This is the time to create a
     dependency manifest and, if dependencies must
     be interrogated at compile-time, this is the
     time to download them as well.")
  (-get-config [scope prj cfg]
    "Applies the provided configuration to any 
     default configuration and returns the
     configuration to use.")
  (-get-module-config [scope prj cfg mod-desc mod-cfg]
    "Applies the provided configuration to any
     default configuration provided by the module
     and returns the configuration to use.")
  (-get-implicit-modules [scope prj cfg]
    "Returns a map of names to modules that the scope
     implicitly provides.")
  (-resolve-name [scope prj config name-parts]
    "Resolves name-parts into a valid name in this scope."))

(defn- scope-with-renv
  [renv]
  ; TODO: we should only use these with-fallbacks if we can't find
  ;       the ideal flow.  We don't want to swallow actual errors.
  (reify Scope
    (-stop [scope prj]
      (renv/stop prj renv))
    (-write-dependencies [scope prj cfg mod-descriptors]
      (renv/invoke-ideal-flow
        prj
        renv
        :org.ulvm.scope/write-dependencies
        {:mod-descriptors mod-descriptors
         :cfg             cfg}))
    (-get-config [scope prj cfg]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-config
          {:config cfg})
        cfg))
    (-get-module-config [scope prj cfg mod-desc mod-cfg]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-module-config
          {:module-descriptors mod-desc
           :cfg                cfg
           :mod-cfg            mod-cfg})
        mod-cfg))
    (-get-implicit-modules [scope prj cfg]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-implicit-modules
          {:cfg cfg})
        {}))
    (-resolve-name [scope prj cfg name-parts]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/resolve-name
          {:config     cfg
           :name-parts name-parts})
        (->> name-parts
             (map name)
             (interpose "_")
             (apply str)
             symbol)))))

(defn stop
  [scope prj]
  (-stop scope prj))

(defn write-dependencies
  [scope prj cfg mod-descriptors]
  (-write-dependencies scope prj cfg mod-descriptors))

(defn get-config
  [scope prj cfg]
  (-get-config scope prj cfg))

(defn get-module-config
  [scope prj cfg mod-desc mod-cfg]
  (-get-module-config scope prj cfg mod-desc mod-cfg))

(defn get-implicit-modules
  [scope prj cfg]
  (-get-implicit-modules scope prj cfg))

(defn resolve-name
  [scope prj config name-parts]
  (-resolve-name scope prj config name-parts))

(defn with-scope-paths
  "Merge the paths for the well-known directories into the scope
   config if they are not already provided."
  [prj scope-name cfg]
  (futil/mlet e/context
              [prj-root    (uprj/get-env prj (k/project-root))
               src-root    (uprj/get-env prj (k/gen-src-root scope-name) "src")
               build-root  (uprj/get-env prj (k/build-root scope-name) "build")
               scope-src   (get cfg ::gen-src-dir scope-name)
               scope-build (get cfg ::build-dir scope-name)]
    (merge
      cfg
      {::gen-src-dir (u/resolve-path prj-root src-root scope-src)
       ::build-dir   (u/resolve-path prj-root build-root scope-build)})))

(defn make-scope
  "Make a scope instance"
  [proj scope-name scope-ent]
  (let [cfg               (->> (get scope-ent ::ucore/config)
                               (with-scope-paths proj scope-name))
        k                 (k/scope-config-keypath scope-name)
        prj               (uprj/set-env proj k cfg)
        {p           :prj
         renv-either :el} (uprj/deref-runnable-env prj scope-ent)]
    (m/extract
      (m/bimap
        (fn [err]
          {:prj   p
           :scope (e/left err)})
        (fn [renv]
          {:prj   (renv/launch p renv [k])
           :scope (e/right (scope-with-renv renv))})
        renv-either))))
