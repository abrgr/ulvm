(ns ^{:author "Adam Berger"} ulvm.blocks
  "Block algorithms"
  (:require [ulvm.utils :as u]
            [cats.monad.either :as e]))

(defn- get-unique-deps
  "Returns the set of deps that are unique to this invocation
   among all invocations in invs"
  [deps inverse-deps invs inv-name]
  (let [remaining-invs (into #{} invs)]
    (->> (get deps inv-name) ; a b
         (filter
           #(->> (get inverse-deps %) ; b c
                 (some remaining-invs)
                 (not)))
         (into #{}))))

(defn- gen-block
  "Generate a block"
  ([deps inverse-deps names]
    (gen-block deps inverse-deps names []))
  ([deps inverse-deps names inner-blocks]
    (let [bindings      (->> names
                             (map #(str "invoke-" %))
                             (interleave (map symbol names))
                             (into []))
          inner-asts    (map :ast inner-blocks)
          direct-deps   (->> names
                             (map #(get deps %))
                             (map #(into [] %))
                             (flatten))
          indirect-deps (->> inner-blocks
                             (map :depends-on)
                             (map #(into [] %))
                             (flatten))
          all-deps      (->> (concat direct-deps indirect-deps)
                             (into #{}))]
      {:provides   names
       :depends-on all-deps
       :ast        `(~'let ~bindings (do ~@inner-asts))})))

(defn build-call-graph
  "Builds the actual call graph for the invocations"
  [prj scope flow-name deps named-invs invocations]
  (let [inverse-deps (u/flip-map deps)]
           ; We walk "up" the invocation graph, beginning with
           ; the invocation that should happen "last."
    (loop [remaining-invs (reverse invocations)
           blocks         []]
            ;At each invocation, labeled inv, we:
      (let [inv-name            (first remaining-invs)
            next-remaining-invs (rest remaining-invs)
            inv                 (get named-invs inv-name)
            ; 1. If we have already created a block in which the
            ;    result of inv is available, we label that
            ;    block.  Otherwise, we create a block, labeled
            ;    block, in which the result of inv is available.
            {possible-blocks  true
             remaining-blocks false} (u/pred-partition #(contains? (:provides %) inv-name) blocks)
            block           (cond
                             (empty? possible-blocks)              (gen-block deps inverse-deps [inv-name])
                             (not (empty? (rest possible-blocks))) (throw "BAD") ; TODO: what do we do here?
                             :else                                 (first possible-blocks))
            ; 2. Gather the set of dependencies of inv that do
            ;    not appear as dependencies of any other
            ;    invocation further "up" the invocation graph. 
            ;    We label these unique-deps.
            unique-deps (get-unique-deps deps inverse-deps (into #{} next-remaining-invs) inv-name)]
              ; unique-deps is the set of dependencies that the
              ; block we are about to create will provide.
              ; We re-label unique-deps provided-vals.
        (let [provided-vals unique-deps
              ; We iterate over all previously-created blocks and
              ; gather those blocks that depend on provided-vals,
              ; including block, created above, labeling this set inner-blocks.
              {inner-blocks     true
               remaining-blocks false } (->> remaining-blocks
                                             (u/pred-partition #(some unique-deps (:depends-on %))))
              result-block  (if (empty? provided-vals)
                              block
                              ; If provided-vals is not empty, we construct a block,
                              ; labeled result-block, within which all values in
                              ; provided-vals are available.
                              ; The body of result-block consists of inner-blocks.
                              ; We say that the dependencies of result-block are the union of the
                              ; dependencies of every item in provided-vals and those dependencies
                              ; of inner-blocks that are not in provided-vals.
                              (gen-block deps inverse-deps provided-vals (conj inner-blocks block)))
              next-blocks  (conj remaining-blocks result-block)]
          (if (empty? next-remaining-invs)
            (e/right (map :ast next-blocks))
            (recur next-remaining-invs next-blocks)))))))
