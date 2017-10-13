(ns ^{:author "Adam Berger"} ulvm.utils
  "ULVM utilities")

(defmacro retrying
  [initial-backoff max-retries retryable-error body]
  `(loop [backoff#           ~initial-backoff
          remaining-retries# ~max-retries
          val#               ~body]
    (if (and (~retryable-error val#) (> remaining-retries# 0))
      (do (Thread/sleep backoff#)
          (recur (* backoff# 2)
                 (dec remaining-retries#)
                 ~body))
      val#)))

(defn- deps-sat?
  [visited deps]
  (every? visited deps))

(defn topo-sort
  ([deps items]
    (topo-sort deps items #{}))
  ([deps items visited]
    (let [item      (first items)
          item-deps (get deps item)
          remaining (rest items)
          deps-sat  (deps-sat? visited item-deps)]
      (cond
        (empty? remaining)
        {:items   [item]
         :visited (conj visited item)
         :unsat   (if deps-sat #{} #{item})}
        deps-sat
        (let [{i :items, v :visited, u :unsat} (topo-sort deps remaining (conj visited item))]
          {:items   (concat [item] i)
           :visited v
           :unsat   u})
        :else
        (let [{i :items, v :visited, u :unsat} (topo-sort deps remaining visited)]
          {:items   (concat i [item])
           :visited (conj v item)
           :unsat   (if (deps-sat? v item-deps) u (conj u item))})))))

(defn flip-map
  "Given a map of keys to colls, return a map from
   each item in a coll to a set of keys that
   map to it."
  [m]
  (reduce
    (fn [flipped [k vs]]
      (merge-with
        #(into #{} (concat %1 %2))
        flipped
        (into {} (map (fn [v] [v #{k}]) vs))))
    {}
    m))

(defn pred-partition
  "Return {true [true-items] false [false-items]}, where
   [true-items] is a vector of items for which
   f(item) is true and similarly for [false-items]."
  [f coll]
  (reduce
    (fn [p i]
      (if (f i)
        (merge-with conj p {true i})
        (merge-with conj p {false i})))
    {true [] false []}
    coll))
