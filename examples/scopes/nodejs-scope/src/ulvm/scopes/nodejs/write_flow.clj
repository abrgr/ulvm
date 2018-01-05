(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-flow
  "NodeJS Flow Writer"
  (:require [clojure.string :as string]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [ulvm.scopes.nodejs.write-file :as write-file]))

(defn- tab-or-spaces
  [cfg]
  (if (= :tabs (get cfg :tabs-or-spaces))
    \tab
    "  "))

(defn- opening-brace
  [cfg]
  (if (get cfg :brace-on-same-line)
    [" {" \newline]
    [\newline (tab-or-spaces cfg) "{"]))

(defn make-js-cfg
  [cfg]
  (merge {:tabs-or-spaces     :spaces
          :brace-on-same-line true}
         cfg))

; we support a subset of the babel ast
; https://babeljs.io/docs/core-packages/babel-type

(defmulti ast-to-js (fn [cfg val] (:type val)))

(defmethod ast-to-js :expression-statement
  [cfg {:keys [expression]}]
  (concat
    (ast-to-js cfg expression)
    [";"]))

(defmethod ast-to-js :assignment-expression
  [cfg {:keys [left right]}]
  (concat
    "const "
    (ast-to-js cfg left)
    [" = "]
    (ast-to-js cfg right)))

(defmethod ast-to-js :identifier
  [cfg {:keys [identifier]}]
  [(name identifier)])

(defmethod ast-to-js :string-literal
  [cfg {:keys [value]}]
  ["\"" (str value) "\""])

(defmethod ast-to-js :boolean-literal
  [cfg {:keys [value]}]
  [(str value)])

(defmethod ast-to-js :numeric-literal
  [cfg {:keys [value]}]
  [(str value)])

(defmethod ast-to-js :array-expression
  [cfg {:keys [elements]}]
  (concat
    ["["]
    (->> elements
         (map ast-to-js cfg)
         (interpose ","))
    ["]"]))

(defmethod ast-to-js :object-expression
  [cfg {:keys [properties]}]
  (concat
    ["{"]
    (->> properties
         (map ast-to-js cfg)
         (interpose ", "))
    ["}"]))

(defmethod ast-to-js :object-property
  [cfg {:keys [key value]}]
  (concat
    (ast-to-js cfg key)
    [": "]
    (ast-to-js cfg key)))

(defmethod ast-to-js :block-statement
  [cfg {:keys [body]}]
  [(opening-brace cfg)
   (tab-or-spaces cfg)
   (->> body
        (map (comp (partial string/join "") flatten (partial ast-to-js cfg)))
        (interpose [\newline (tab-or-spaces cfg)]))
   \newline
   "}" \newline])

(defmethod ast-to-js :function-declaration
  [cfg {:keys [id params body]}]
  ["function "
   (ast-to-js cfg id)
   "("
   (->> params
        (map ast-to-js cfg)
        (interpose ", "))
   ")"
   (ast-to-js cfg body)])

(defmethod ast-to-js :call-expression
  [cfg {:keys [callee arguments]}]
  (concat
    (ast-to-js cfg callee)
    ["("]
    (interpose ", " (map ast-to-js cfg arguments))
    [")"]))

(defn wrap-in-fn
  [fname params body-ast]
  {:type   :function-declaration
   :id     {:type       :identifier
            :identifier (name fname)}
   :body   {:type :block-statement
            :body body-ast}
   :params (for [p params]
                {:type :identifier
                 :identifier p})})

(defn to-js-str
  [js-vec]
  (string/join "" js-vec))

(defn f
  "Write this scope's portion of flow based on the flow-ast."
  [cfg flow-name flow-args flow-ast]
  (let [filename   (str (name flow-name) ".js") ; TODO: sanitize flow-name
        path       (-> (get cfg :ulvm.scopes/gen-src-dir)
                       (io/file "flows" filename)
                       (.getPath))]
    (->> flow-ast
         (wrap-in-fn flow-name flow-args)
         (ast-to-js (make-js-cfg (get cfg :js-cfg)))
         flatten
         to-js-str
         (write-file/w path))))
