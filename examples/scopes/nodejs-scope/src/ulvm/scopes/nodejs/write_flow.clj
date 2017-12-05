(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-flow
  "NodeJS Flow Writer"
  (:require [clojure.string :as string]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [ulvm.scopes.nodejs.write-file :as write-file]))

; we support a subset of the babel ast
; https://babeljs.io/docs/core-packages/babel-type

(defmulti ast-to-js :type)

(defmethod ast-to-js :expression-statement
  [{:keys [expression]}]
  (concat
    (ast-to-js expression)
    [";"]))

(defmethod ast-to-js :assignment-expression
  [{:keys [left right]}]
  (concat
    "const "
    (ast-to-js left)
    [" = "]
    (ast-to-js right)))

(defmethod ast-to-js :identifier
  [{:keys [identifier]}]
  [(name identifier)])

(defmethod ast-to-js :string-literal
  [{:keys [value]}]
  ["\"" (str value) "\""])

(defmethod ast-to-js :boolean-literal
  [{:keys [value]}]
  [(str value)])

(defmethod ast-to-js :numeric-literal
  [{:keys [value]}]
  [(str value)])

(defmethod ast-to-js :array-expression
  [{:keys [elements]}]
  (concat
    ["["]
    (->> elements
         (map ast-to-js)
         (interpose ", "))
    ["]"]))

(defmethod ast-to-js :object-expression
  [{:keys [properties]}]
  (concat
    ["{"]
    (->> properties
         (map ast-to-js)
         (interpose ", "))
    ["}"]))

(defmethod ast-to-js :object-property
  [{:keys [key value]}]
  (concat
    (ast-to-js key)
    [": "]
    (ast-to-js key)))

(defmethod ast-to-js :call-expression
  [{:keys [callee arguments]}]
  (concat
    (ast-to-js callee)
    ["("]
    (interpose ", " (map ast-to-js arguments))
    [")"]))

(defn to-js
  [ast]
  (->> ast
      (map (comp (partial string/join "") flatten ast-to-js))
      (string/join \newline)))

(defn f
  "Write this scope's portion of flow based on the flow-ast."
  [cfg flow-name flow-ast]
  (let [filename   (str (name flow-name) ".js") ; TODO: sanitize flow-name
        path       (-> (get cfg :ulvm.scopes/gen-src-dir)
                       (io/file "flows" filename)
                       (.getPath))]
    (write-file/w path (to-js flow-ast))))
