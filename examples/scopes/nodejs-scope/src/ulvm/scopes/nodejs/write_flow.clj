(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-flow
  "NodeJS Flow Writer"
  (:require [clojure.string :as string]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [ulvm.scopes.nodejs.write-file :as write-file]))

(defn f
  "Write this scope's portion of flow based on the flow-ast."
  [cfg flow-name flow-ast]
  (let [filename   (str (name flow-name) ".js") ; TODO: sanitize flow-name
        path       (-> (get cfg :ulvm.scopes/gen-src-dir)
                       (io/file "flows" filename)
                       (.getPath))]
    (write-file/w path (pr-str flow-ast))))
