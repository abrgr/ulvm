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
  (-write-dependencies [scope prj mod-descriptors]
    "Writes scope dependencies given the set of module
     descriptors.  This is the time to create a
     dependency manifest and, if dependencies must
     be interrogated at compile-time, this is the
     time to download them as well.")
  (-get-module-config [scope prj mod-desc mod-cfg]
    "Applies the provided configuration to any
     default configuration provided by the module
     and returns the configuration to use.")
  (-get-implicit-modules [scope prj]
    "Returns a map of names to modules that the scope
     implicitly provides.")
  (-resolve-name [scope prj name-parts]
    "Resolves name-parts into a valid name in this scope.")
  (-write-flow [scope prj flow-name flow-args flow-ent flow-ast]
    "Writes this scope's portion of flow.")
  (-get-name [scope]
    "Retrieves the name of the scope.")
  (-get-config [scope]
    "Retrieves the config for the scope."))

(defn- scope-with-renv
  [renv scope-name cfg]
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
        {:mod-descriptors mod-descriptors
         :cfg             cfg}))
    (-get-module-config [scope prj mod-desc mod-cfg]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-module-config
          {:module-descriptors mod-desc
           :cfg                cfg
           :mod-cfg            mod-cfg})
        mod-cfg))
    (-get-implicit-modules [scope prj]
      (futil/with-fallback
        (renv/invoke-ideal-flow
          prj
          renv
          :org.ulvm.scope/get-implicit-modules
          {:cfg cfg})
        {}))
    (-resolve-name [scope prj name-parts]
      (futil/with-fallback
        (m/->>= (e/right {:cfg        cfg
                          :name-parts name-parts})
                (renv/invoke-ideal-flow
                  prj
                  renv
                  :org.ulvm.scope/resolve-name)
                ((comp m/return :name first)))
        (->> name-parts
             (map name)
             (interpose "_")
             (apply str)
             symbol)))
    (-write-flow [scope prj flow-name flow-args flow-ent flow-ast]
      (renv/invoke-ideal-flow
        prj
        renv
        :org.ulvm.scope/write-flow
        {:cfg       cfg
         :flow-name flow-name
         :flow-args flow-args
         :flow-ent  flow-ent
         :flow-ast  flow-ast}))
    (-get-name [scope]
      scope-name)
    (-get-config [scope]
      cfg)))

(defn stop
  [scope prj]
  (-stop scope prj))

(defn write-dependencies
  [scope prj mod-descriptors]
  (-write-dependencies scope prj mod-descriptors))

(defn get-module-config
  [scope prj mod-desc mod-cfg]
  (-get-module-config scope prj mod-desc mod-cfg))

(defn get-implicit-modules
  [scope prj]
  (-get-implicit-modules scope prj))

(defn resolve-name
  [scope prj name-parts]
  (-resolve-name scope prj name-parts))

(defn write-flow
  [scope prj flow-name flow-args flow-ent flow-ast]
  (-write-flow scope prj flow-name flow-args flow-ent flow-ast))

(defn get-name
  [scope]
  (-get-name scope))

(defn get-config
  [scope]
  (-get-config scope))

(defn with-default-scope-cfg
  "Merge the default scope config into the scope config if they are not
   already provided."
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
       ::build-dir   (u/resolve-path prj-root build-root scope-build)
       ::scope-name  scope-name
       ::fs-secret   (java.util.UUID/randomUUID)})))

(defn- get-cfg
  [prj renv cfg]
  (futil/with-fallback
    (renv/invoke-ideal-flow
      prj
      renv
      :org.ulvm.scope/get-config
      {:config cfg})
    cfg))

(defn make-scope
  "Make a scope instance"
  [proj scope-name scope-ent]
  (let [cfg               (->> (get scope-ent ::ucore/config)
                               (with-default-scope-cfg proj scope-name))
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
          (let [prj (renv/launch p renv [k])]
            {:prj   prj
             :scope (e/right (scope-with-renv renv scope-name (get-cfg prj renv cfg)))}))
        renv-either))))
