(ns ^{:author "Adam Berger"} ulvm.docker-utils
  "ULVM Docker utilities"
  (:require [ulvm.project :as uprj]
            [ulvm.func-utils :as futil]
            [cats.monad.either :as e])
  (:import  [com.spotify.docker.client DefaultDockerClient
                                       ProgressHandler
                                       DockerClient$LogsParam
                                       LogMessage$Stream]
            [com.spotify.docker.client.messages RegistryAuth
                                                ContainerConfig]))

(defn- make-registry-auth
  [registry-auth]
  (RegistryAuth/create (:username       registry-auth)
                       (:password       registry-auth)
                       (:email          registry-auth)
                       (:server-address registry-auth)
                       (:identity-token registry-auth)
                       (:auth           registry-auth)))

(defn docker-client
  "Get a docker client"
  [prj]
  (futil/mlet e/context
              [uri  (uprj/get-env prj [::docker-host] (e/right "unix:///var/run/docker.sock"))
               auth (uprj/get-env prj [::registry-auth])]
    (cond-> (DefaultDockerClient/builder)
            true         (.uri uri)
            (some? auth) (.registryAuth (make-registry-auth auth))
            true         (.build))))

(defn pull-image
  "Pull a docker image"
  [prj image]
  (let [progress  (reify ProgressHandler
                    (progress [this msg] nil))]
    (futil/mlet e/context
                [client (docker-client prj)
                 _ (e/try-either (.pull client image progress))]
      (e/right {:image image}))))

(defn- add-port-specs
  [builder ports]
  (.portSpecs builder 
              (map (fn [[container-port host-port]]
                       (str (container-port)
                            ":"
                            (host-port)))
                   ports)))

(defn- add-env
  [builder envs]
  (.env builder
        (map (fn [[var-name value]]
                 (str var-name "=" value))
             envs)))

(defn- add-volumes
  [builder vols]
  (.volumes builder
            (into {}
                  (map (fn [spec]
                           [(:container spec) (:host spec)])
                       vols))))

(defmacro -|>
  "Given forms like (method first-arg), invokes
   (method obj first-arg) if first-arg is non-nil.
   obj for each invocation is taken to be the result
   of the prior invocation.
   
   Useful for invoking methods on a Java builder iff
   the argument to the builder method is present."
  [obj & forms]
  (concat
    `(cond-> ~obj)
    (mapcat (fn [f]
           (let [val (second f)]
             `((some? ~val) ~f)))
       forms)))

(defn- create-container-cfg
  [desc]
  (.build
    (-|> (ContainerConfig/builder)
         (.image           (:image desc))
         (add-port-specs   (:ports desc))
         (add-env          (:env desc))
         (.cmd             (:cmd desc))
         (.entrypoint      (:entrypoint desc))
         (.labels          (:labels desc))
         (.workingDir      (:working-dir desc))
         (.networkDisabled (:network-disabled desc))
         (add-volumes      (:volumes desc)))))

(defn create-container
  "Create a docker container"
  [prj desc]
  (futil/mlet e/context
              [client       (docker-client prj)
               cfg          (create-container-cfg desc)
               container    (e/try-either (.createContainer client cfg))
               container-id (.id container)
               _            (e/try-either (.startContainer client container-id))]
    (e/right {:container-id container-id})))

(defn- read-buffer
  [buf]
  (loop [s []]
    (if (pos? (.remaining buf))
      (recur (conj s (char (.get buf))))
      (apply str s))))

(defn- log-seq
  [log-stream]
  (lazy-seq
    (if (.hasNext log-stream)
      (let [msg     (.next log-stream)
            stream  (.stream msg)
            content (read-buffer (.content msg))
            log     {:content content
                     :stream  (when
                                (= stream (LogMessage$Stream/STDOUT)) :stdout
                                (= stream (LogMessage$Stream/STDERR)) :stderr
                                :else                                 :unknown)}]
        (cons log (log-seq log-stream)))
      nil)))

(defn get-container-output
  "Returns an either of a lazy seq of container output lines"
  [prj container-id]
  (futil/mlet e/context
              [client          (docker-client prj)
               output-selector (into-array DockerClient$LogsParam
                                           [(DockerClient$LogsParam/stdout)
                                            (DockerClient$LogsParam/stderr)
                                            (DockerClient$LogsParam/follow true)])
               log-stream      (.logs client container-id output-selector)
               logs            (e/try-either (log-seq log-stream))]
    (e/right logs)))
