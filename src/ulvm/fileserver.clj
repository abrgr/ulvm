(ns ^{:author "Adam Berger"} ulvm.fileserver
  "Fileserver"
  (:require [org.httpkit.server :as h]
            [compojure.core :as c]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [cats.core :as m]
            [cats.monad.either :as e]
            [ulvm.aws-auth :as aws-auth]
            [ulvm.project :as uprj]
            [ulvm.func-utils :as futil]
            [ulvm.env-keypaths :as k])
  (:import  [java.net URI]))

(defn- read-all
  [rdr size]
  (let [buf (char-array size)]
    (loop [offset 0]
      (let [bytes-read (.read rdr buf offset (- size offset))
            total-read (+ offset bytes-read)]
        (cond
          (< bytes-read 0) buf
          (< total-read size) (recur total-read)
          :else buf)))))

(defn- next-chunk
  "Reads chunked s3 uploads as described in:
   http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-streaming.html#sigv4-chunked-body-definition"
  [rdr]
  (let [preamble       (.readLine rdr)
        [hex-size sig] (string/split preamble #";")
        size           (Integer/parseInt hex-size 16)]
    (if (= 0 size)
      nil
      (read-all rdr size))))
        
(defn- store-obj
  "Store an object"
  [scopes {:keys [body verified-path]}]
  (let [file (io/file verified-path)
        rdr  (io/reader body)]
    (->> file
         (.getParentFile)
         (.mkdirs))
    (with-open [w (io/writer file)]
      (loop [buf (next-chunk rdr)]
        (when (some? buf)
          (.write w buf 0 (count buf)))))
    {:status 200}))

(defn- get-obj
  "Get an object"
  [scopes {:keys [verified-path]}]
  {:status 200
   :body  (io/file verified-path)})

(defn- named-delimited-parts
  [name delim s]
  (-> s
      (string/replace-first (re-pattern (str "^" name)) "")
      (string/split (re-pattern delim))))

(defn- auth-parts
  [auth]
  (let [[method cred ssv-signed-headers sig]   (string/split auth #" |, ")
        [access-key date region service _]     (named-delimited-parts "Credential=" "/" cred)
        signed-headers                         (named-delimited-parts "SignedHeaders=" ";" ssv-signed-headers)]
    {:method         method
     :access-key     access-key
     :date           date
     :region         region
     :service        service
     :signed-headers signed-headers
     :sig            (string/replace-first sig #"^Signature=" "")}))

(defn- resolve-bucket
  [req]
  (->> (get-in req [:headers "host"])
       (re-matches #"^([^.]+)[.].*")
       (second)))

(defn- path-prefix?
  [prefix path]
  (if (or (nil? prefix)
          (nil? path))
    false
    (string/starts-with? path prefix)))

(defn- with-verified-req
  [prj req scope-name app]
  (futil/mlet e/context
              [scopes         (get-in prj [:entities :ulvm.core/scopes])
               scope          (get scopes scope-name)
               scope-cfg      (uprj/get-env prj (k/scope-config-keypath scope-name))
               path           (-> req
                                  (:uri)
                                  (URI.)
                                  (.getPath)
                                  (io/file)
                                  (.getCanonicalPath))
               expected-src   (get scope-cfg :ulvm.scopes/gen-src-dir)
               expected-build (get scope-cfg :ulvm.scopes/build-dir)
               valid-path?    (or (path-prefix? expected-src path)
                                  (path-prefix? expected-build path))]
    (if (and (some? scope)
             valid-path?)
      (e/right
        (app (merge req {:verified-scope scope
                         :verified-path  path})))
      (e/left "Unauthorized"))))

(defn- verify-auth
  [prj app]
  (fn [req]
    (m/extract
      (m/bimap
        #(do (println "Unauthorized request" req %)
             {:status 403
              :body   "Unauthorized"})
        identity
        (futil/mlet e/context
                    [{:keys [headers
                              uri
                              request-method]} req
                     auth-header               (get headers "authorization")
                     auth                      (auth-parts auth-header)
                     req-method                (-> request-method
                                                   name
                                                   string/upper-case)
                     bucket                    (resolve-bucket req)
                     scope-name                (keyword bucket)
                     scope-cfg                 (uprj/get-env
                                                 prj
                                                 (k/scope-config-keypath
                                                   scope-name))
                     secret                    (get scope-cfg :ulvm.scopes/fs-secret)
                     calculated-auth           (aws-auth/aws4-auth
                                                 req-method
                                                 uri
                                                 headers
                                                 (get auth :signed-headers)
                                                 (get auth :region)
                                                 "s3"
                                                 (get auth :access-key)
                                                 secret)]
            (if (= calculated-auth auth-header)
              (with-verified-req prj req scope-name app)
              (e/left "Unauthorized")))))))

(defn- routes
  [prj]
    (c/routes
      (c/POST "/*" [] (partial store-obj prj))
      (c/PUT  "/*" [] (partial store-obj prj))
      (c/GET  "/*" [] (partial get-obj prj))))

(defn- app
  [prj]
  (->>  (routes prj)
        (verify-auth prj)))

(defn start-server
  [prj]
  (futil/mlet e/context
              [uri  (uprj/get-env prj (k/fileserver-base-uri))
               port (->> uri
                         (URI.)
                         (.getPort))]
    (e/right (h/run-server (app prj) {:port port}))))

(defmacro with-fs
  "Runs a local fileserver to accept requests from any scopes
   defined in project. Returns a function that will stop the
   fileserver."
  [prj & forms]
  `(futil/mlet e/context
               [stop-server# (start-server ~prj)
                res#         (do ~@forms)]
     (stop-server#)
     (futil/lift-either res#)))
