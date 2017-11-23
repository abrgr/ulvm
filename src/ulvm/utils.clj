(ns ^{:author "Adam Berger"} ulvm.utils
  "ULVM utilities"
  (:require [clojure.java.io :as io])
  (:refer-clojure :rename {get-in core-get-in}))

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
        (empty? items)
        {:items   []
         :visited #{}
         :unsat   #{}}
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

(defn- coll-get
  "Treats coll [:key1 :val1 :key2 :val2] as a map and gets the
   value corresponding tot he desired key."
  [m k]
  (->> m
       (partition 2)
       (filter #(= k (first %)))
       ; first filter result
       first
       ; value of the (key, value) tuple
       second))

(defn get-in
  "Get-in for generalized maps (either maps or colls of map
   entries."
  [m ks]
  (loop [k (first ks)
         r (rest ks)
         c m]
    (cond
      (nil? k) c
      (nil? c) nil
      (map? c) (recur (first r) (rest r) (get c k))
      :else    (recur (first r) (rest r) (coll-get c k)))))

(defn map-to-vals
  "Given keys and a val function, return a map from each key
   to the corresponding val."
  [val-fn keys]
  (->> keys
       (map
         (fn [k]
           [k (val-fn k)]))
       (into {})))

(defn resolve-path
  "Starting with the last absolute path, append the rest of the
   paths as relative paths.
   
   For example, (resolve-path \"/a\" \"b\" \"/c\" \"d\") yields
   a java.io.File with path \"/c/d\"."
  [& paths]
  (->> (reverse paths)
       (map name)
       (reduce
         (fn [{:keys [res path-queue]} path]
           (cond
             (some? res)       {:res res}
             (->> path
                  io/file
                  .isAbsolute) {:res (concat [path] (reverse path-queue))}
             :else             {:path-queue (conj path-queue path)}))
         {:path-queue [], :res nil})
       ((fn [{:keys [res path-queue]}]
          (or res path-queue)))
       (apply io/file)))
