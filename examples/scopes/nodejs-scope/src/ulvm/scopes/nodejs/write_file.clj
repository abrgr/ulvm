(ns ^{:author "Adam Berger"} ulvm.scopes.nodejs.write-file
  "Utility to write files"
  (:require [clojure.string :as string]
            [amazonica.aws.s3 :as s3]))

(defn w
  "Write a file"
  [path contents]
  (let [stream     (java.io.ByteArrayInputStream. (.getBytes contents "utf-8"))
                   ; s3 keys do not start with a slash
        k          (string/replace-first path #"^[/]" "")
        bucket     (System/getenv "SCOPE_NAME")]
    (s3/put-object
      {:access-key  bucket
       :secret-key  (System/getenv "SECRET_KEY")
       :endpoint    (System/getenv "FS_BASE_URI")}
      :key          k
      :bucket-name  bucket
      :input-stream stream)))
