(ns ^{:author "Adam Berger"} ulvm.mod-combinators.js.sync.ast
  "Synchronous Javascript Module Combinator AST Generation")

(defn- get-arg-positions
  [mod]
        ; arg-mappings is a list of lists of args where each
        ; inner list represents a single overload
  (let [arg-mappings (get-in mod [:ulvm.core/config
                                  :ulvm.arg-mappings/positional])]
    ; turn [[:a :b] [:c :d]] into {:a 0, :b 1, :c 0, :d 1}
    (apply
      merge
      (->> arg-mappings
           (map #(map vector % (range)))
           (map (partial into {}))))))

(defmulti to-js-ast type)

(defmethod to-js-ast clojure.lang.Symbol
  [id]
  {:type       :identifier
   :identifier (name id)})

(defmethod to-js-ast java.lang.String
  [s]
  {:type  :string-literal
   :value s})

(defmethod to-js-ast clojure.lang.Keyword
  [k]
  {:type  :string-literal
   :value (name k)})

(defmethod to-js-ast java.lang.Boolean
  [b]
  {:type  :boolean-literal
   :value b})

(defmethod to-js-ast java.lang.Long
  [n]
  {:type  :numeric-literal
   :value n})

(defmethod to-js-ast java.lang.Double
  [n]
  {:type  :numeric-literal
   :value n})

(defmethod to-js-ast clojure.lang.PersistentVector
  [v]
  {:type     :array-expression
   :elements (map to-js-ast v)})

(defmethod to-js-ast clojure.lang.PersistentList
  [v]
  {:type     :array-expression
   :elements (map to-js-ast v)})

(defmethod to-js-ast clojure.lang.PersistentHashSet
  [v]
  {:type     :array-expression
   :elements (map to-js-ast v)})

(defmethod to-js-ast clojure.lang.PersistentArrayMap
  [m]
  {:type       :object-expression
   :properties (map
                 (fn [[k, v]]
                   {:type  :object-property
                    :key   (to-js-ast (name k))
                    :value (to-js-ast k)})
                 m)})

(defn- gen-arg-ast
  [mod arg-names args]
  (let [arg-pos (get-arg-positions mod)]
    (->> args
         ; get a list of maps with :pos and :val
         (map
           (fn [[n arg]]
             {:pos (get arg-pos n)
                       ; arg looks like
                       ; [:data {}] or
                       ; [:ref [:named-ref-arg {:sub-result :username, :result authorized-login}]
              :val (if (= (first arg) :data)
                       ; if we have data, we assume it's a valid ast
                       (second arg)
                       ; if we're using a reference, we look up the name
                       (get arg-names n))}))
         ; sort by :pos
         (sort-by :pos)
         ; get a list of vals in order
         (map :val)
         ; translate them into the obvious asts
         (map to-js-ast)
         (into []))))

(defn- gen-inv-ast
  [{:keys [result-names arg-names mod mod-name inv]}]
  (let [result-name (get result-names :*default*)
        args        (get inv :args)]
    {:type       :expression-statement
     :expression {:type  :assignment-expression
                  :left  {:type       :identifier
                          :identifier result-name}
                  :right {:type      :call-expression
                          :callee    {:type       :identifier
                                      :identifier mod-name}
                          :arguments (gen-arg-ast mod arg-names args)}}}))

(defn gen
  "Generates a js ast for the given invocations"
  [{:keys [invocations body]}]
  ; TODO: can also return :err
  {:ast (concat
          (map gen-inv-ast invocations)
          body)})
