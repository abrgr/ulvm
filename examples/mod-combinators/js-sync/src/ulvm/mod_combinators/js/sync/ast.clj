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

(defn- gen-arg-ast
  [mod arg-names args]
  (let [arg-pos (get-arg-positions mod)]
    (->> args
         ; get a list of maps with :pos and :val
         (map
           (fn [[name arg]]
             {:pos (get arg-pos name)
                       ; arg looks like
                       ; [:data {}] or
                       ; [:ref [:named-ref-arg {:sub-result :username, :result authorized-login}]
              :val (if (= (first arg) :data)
                       ; if we have data, we assume it's a valid ast
                       (second arg)
                       ; if we're using a reference, we look up the name
                       (get arg-names name))}))
         ; sort by :pos
         (sort-by :pos)
         ; get a list of vals in order
         (map :val))))

(defn- gen-inv-ast
  [{:keys [result-names arg-names mod mod-name inv]}]
  (let [result-name (get result-names :*default*)
        args        (get inv :args)]
    `(:assign
       ~result-name
       (:invoke
         ~mod-name
         ~@(gen-arg-ast mod arg-names args)))))

(defn gen
  "Generates a js ast for the given invocations"
  [{:keys [invocations body]}]
  ; TODO: can also return :err
  {:ast (concat
          (map gen-inv-ast invocations)
          body)})
