(ns ^{:author "Adam Berger"} ulvm.call-graph-transforms.de-nest
  "De-nesting transform"
  (:refer-clojure :exclude [==])
  (:use     [clojure.core.logic]))

(declare results-in-same-block
         de-nest
         de-nest-many
         transform)

(defne results-in-same-block
  [in-same-block invs]
  ([_ []])
  ([in-same-block [?h . ?t]]
    (membero ?h in-same-block)
    (results-in-same-block in-same-block ?t)))

(defne vappendo
  "appendo that supports vectors"
  [x y z]
  ([x [] x])
  ([x [h . t] z]
    (fresh [i]
      (conjo x h i)
      (vappendo i t z))))

(defne de-nest
  [in-same-block input-block output-block]
  ([_
    {:provides   ?provides
     :depends-on ?deps
     :body       []}
    [{:provides   ?provides
      :depends-on ?deps
      :body       []}]])
  ([in-same-block
    {:provides   ?provides
     :depends-on ?deps
     :body       ?body}
    output-blocks]
    (conde
      [(fresh [inner output-t h-provides h-deps h-body]
         (results-in-same-block in-same-block ?provides)
         (matche [?body]
           ([[?h . ?t]]
             (== {:provides   h-provides
                  :depends-on h-deps
                  :body       h-body}
                 ?h)
             (vappendo ?t h-body inner)
             (de-nest in-same-block
                     {:provides   h-provides
                      :depends-on h-deps
                      :body       inner}
                     output-t)
             (vappendo [{:provides   ?provides
                         :depends-on ?deps
                         :inner      []}]
                       output-t
                       output-blocks))))]
      [(fresh [de-nested]
         (nafc results-in-same-block in-same-block ?provides)
         (nafc emptyo ?body)
         (de-nest-many in-same-block ?body de-nested)
         (== output-blocks
             [{:provides   ?provides
               :depends-on ?deps
               :body       de-nested}]))])))

(defne de-nest-many
  [in-same-block input-blocks output-blocks]
  ([_ [] []])
  ([in-same-block [h . t] output-blocks]
    (fresh [de-nested-h de-nested-t]
      (de-nest in-same-block h de-nested-h)
      (de-nest-many in-same-block t de-nested-t)
      (vappendo de-nested-h de-nested-t output-blocks))))

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
  [deps graph-config named-invs call-graph]
  (let [in-same-block (reduce
                        (fn [in-same-block [name inv]]
                          (let [mc nil ; TODO: get the mod-combinator for inv
                                mc-attrs (get-in call-graph [:mod-combinator-configs mc :attrs])]
                            (if (contains? mc-attrs :ulvm.core/result-in-invocation-block)
                              (conj in-same-block name)
                              in-same-block)))
                        []
                        named-invs)]
    (->>
      (run 1 [q]
        (de-nest-many [:a :b] call-graph q))
      first)))
