(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.resolve-name
  "Name resolution for the NodeJS scope"
  (:require [clojure.string :as string]))

(defn- escape
  "Need to turn a valid clojure identifier into a valid js identifier"
  [part]
  (-> part
      (string/replace #"-" "_")
      (string/replace #"^[^a-zA-Z]" "v_")
      ; TODO: this is slightly dangerous since it we may map multiple
      ;       raw inputs to the same escaped identifier
      (string/replace #"[^a-zA-Z0-9_]" "_")))

(defn n
  "Convert an array of name-parts that each conform to clojure identifier
   rules into a single valid javascript identifier"
  [name-parts]
  (->> name-parts
       (interpose "_") ; TODO: same danger as in escape
       (apply str)
       escape
       symbol))
