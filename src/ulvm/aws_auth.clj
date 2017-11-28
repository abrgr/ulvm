(ns ^{:author "Adam Berger"} ulvm.aws-auth
  "AWS Authentication Utils"
  (:require [clojure.string :as string])
  (:import  [javax.crypto Mac]
            [javax.crypto.spec SecretKeySpec]
            [java.security MessageDigest]))

; hex conversion from: https://stackoverflow.com/questions/9655181/how-to-convert-a-byte-array-to-a-hex-string-in-java

(def ^{:private true} hex-chars
  (char-array "0123456789ABCDEF"))

(defn- as-hex [bytes]
  )

(defn- ^String as-hex-str [bytes]
  (->> bytes
       (map #(format "%02x" (if (neg? %) (bit-and % 0xFF) %)))
       (apply str)))

; Essentially lifted from: http://sapient-pair.com/blog/2016/03/08/clojure-aws4-auth/

(defn- ^bytes to-utf8 [s]
  (.getBytes (str s) "utf-8"))

(defn- ^String sha-256 [bs]
  (let [sha (MessageDigest/getInstance "SHA-256")]
    (.update sha ^bytes bs)
    (as-hex-str (.digest sha))))

(defn- hmac-256 [secret-key s]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. secret-key "HmacSHA256"))
    (.doFinal mac (to-utf8 s))))

(defn- stringify-headers [headers]
  (let [s (StringBuilder.)]
    (doseq [[k v] headers]
      (doto s
        (.append k)
        (.append ":")
        (.append v)
        (.append "\n")))
    (.toString s)))

(defn- aws4-auth-canonical-request [method uri canonical-headers]
  (str
    method \newline
    uri    \newline
    ""     \newline ; query string
    (stringify-headers canonical-headers)      \newline
    (string/join ";" (keys canonical-headers)) \newline
    (get canonical-headers "x-amz-content-sha256" "")))

(defn- aws4-auth-canonical-headers
  [headers signed-headers]
  (->> (select-keys headers signed-headers)
       (map
         (fn [[k v]]
           [(string/lower-case (name k)) (string/trim v)]))
        (into (sorted-map))))

(defn- string-to-sign
  [timestamp short-timestamp region service method uri canonical-headers]
  (str
    "AWS4-HMAC-SHA256\n"
    timestamp "\n"
    short-timestamp "/" region "/" service "/aws4_request" "\n"
    (->> (aws4-auth-canonical-request method uri canonical-headers)
         to-utf8
         sha-256)))

(defn aws4-auth
  "Get the aws v4 authorization string"
  [method uri headers signed-headers region service access-key-id secret-key]
  (let [canonical-headers (aws4-auth-canonical-headers headers signed-headers)
        timestamp         (get canonical-headers "x-amz-date")
        short-timestamp   (.substring ^String timestamp 0 8)
        s                 (string-to-sign
                            timestamp
                            short-timestamp
                            region
                            service
                            method
                            uri
                            canonical-headers)
        signing-key (-> (str "AWS4" secret-key)
                        (to-utf8)
                        (hmac-256 short-timestamp)
                        (hmac-256 region)
                        (hmac-256 service)
                        (hmac-256 "aws4_request"))
        signature (hmac-256 signing-key s)]
    (str
      "AWS4-HMAC-SHA256 "
      "Credential=" access-key-id "/" short-timestamp "/" region "/" service
      "/aws4_request, "
      "SignedHeaders=" (string/join ";" signed-headers) ", "
      "Signature=" (as-hex-str signature))))
