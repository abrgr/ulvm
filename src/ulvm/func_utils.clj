(ns ^{:author "Adam Berger"} ulvm.func-utils
  "Project definition"
  (:require [clojure.spec :as s]
            [ulvm.core :as ucore]
            [ulvm.spec-utils :as su]
            [cats.context :as c]
            [cats.protocols :as p]
            [cats.monad.either :as e]
            [cats.core :as m]))

(declare lift-either
         get-in-either-recur
         get-in-either
         mlet)

(defn lift-either
  [x]
  (if (e/either? x)
    x
    (e/right x)))

(s/fdef lift-either
        :args (s/cat :x su/any)
        :ret  e/right?)

(defmacro mlet
  [ctx bindings & body]
  (when-not (and (vector? bindings)
                 (not-empty bindings)
                 (even? (count bindings)))
    (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
  (->> (reverse (partition 2 bindings))
       (reduce (fn [acc [l r]]
                 (case l
                   :let  `(let ~r ~acc)
                   :when `(m/bind (m/guard ~r)
                            (fn [~(gensym)] ~acc))
                   `(let [r# ~r]
                      (m/bind (if (and (satisfies? p/Contextual r#)
                                       (some? (p/-get-context r#)))
                                r#
                                (m/pure ~ctx r#))
                              (fn [~l] ~acc)))))
               `(do ~@body))))

(defn get-in-either
  [f key-path not-found]
  (mlet e/context
        [key           (first key-path)
         next-key-path (rest key-path)
         form          f
         has-key       (contains? form key)
         val           (if has-key (get form key) not-found)]
    (if (or (not has-key)
            (empty? next-key-path))
      (lift-either val)
      (get-in-either val next-key-path not-found))))

(s/fdef get-in-either
        :args (s/cat :f         su/any
                     :key-path  sequential?
                     :not-found su/any)
        :ret  su/any
        :fn   (fn [{args :args ret :ret}]
                  (or (= ret (:not-found args))
                      (e/either? ret))))
