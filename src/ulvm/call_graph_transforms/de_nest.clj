(ns ^{:author "Adam Berger"} ulvm.call-graph-transforms.de-nest
  "De-nesting transform"
  (:require [ulvm.core :as ucore]
            [clojure.zip :as z]))

(defn- block-zip
  "Zipper for blocks"
  [root]
  (z/zipper
   #(not-empty (get % :body))
   #(get % :body)
   (fn [x children]
     (assoc x :body (vec children)))
   root))

(defn- z-insert-all
  [loc nodes]
  (if (empty? nodes)
    loc
    (recur (z/insert-right loc (first nodes))
           (rest nodes))))
    
(defn- de-nest
  [in-same-block blocks]
  ; we wrap blocks in a top-level block so that we can de-nest
  ; our actual top-level blocks
  (loop [loc (z/next
               (block-zip {:provides   #{}
                           :invs       {}
                           :depends-on #{}
                           :body       blocks}))]
    (if (z/end? loc)
      (:body (z/root loc)) ; un-wrap our top-level block
      (let [n    (z/node loc)
            {body     :body
             deps     :depends-on
             invs     :invs
             provides :provides}   n]
        (->
          (if (in-same-block provides)
            (-> loc
              (z-insert-all body)
              (z/replace
                {:provides   provides
                 :invs       invs
                 :depends-on deps
                 :body       []}))
            loc)
          z/next
          recur)))))

(defn transform
  "Most invocations (e.g. synchronous calls in most languages)
   will provide their result in the same block in which they
   were called.  This transformer removes unnecessary nesting
   in those situations.

   For example, given a graph like this:
   [{:provides #{\"a\"},
     :depends-on #{},
     :body
       [{:provides #{\"b\"},
         :depends-on #{\"a\"},
         :body []}]}]
   We will transform it to the following if a provides its result
   in the same block in which it's called:
   [{:provides #{\"a\"},
     :depends-on #{}}
    {:provides #{\"b\"},
     :depends-on #{\"a\"},
     :body []}]"
  [deps scope-name graph-cfg named-invs call-graph]
  (letfn [(in-same-block [invs]
            (every?
              #(as-> (get-in named-invs [%
                                         :mod
                                         ::ucore/mod-combinator-name]) m
                     (get-in graph-cfg [:mod-combinator-cfgs
                                        scope-name
                                        m
                                        :attrs
                                        ::ucore/result-in-invocation-block]))
              invs))]
     (de-nest in-same-block call-graph)))
