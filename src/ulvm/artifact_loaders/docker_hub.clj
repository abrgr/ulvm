(ns ^{:author "Adam Berger"} ulvm.artifact-loaders.docker-hub
  "ULVM builtin Docker Hub loader"
  (:require [org.httpkit.client :as http-kit]
            [ulvm.mod-loaders :as l]
            [ulvm.project :as uprj]
            [ulvm.runnable-envs :as renv]
            [cats.monad.either :as e])
  (:import com.spotify.docker.client.DefaultDockerClient
           com.spotify.docker.client.messages.RegistryAuth))

(declare pull-image)

(defmethod renv/builtin-load-artifact :ulvm.artifact-loaders/docker-hub
  [prj _ desc]
  (pull-image prj desc))

(defn- make-registry-auth
  [registry-auth]
  (RegistryAuth/create (:username       registry-auth)
                       (:password       registry-auth)
                       (:email          registry-auth)
                       (:server-address registry-auth)
                       (:identity-token registry-auth)
                       (:auth           registry-auth)))

(defn- docker-client
  "Get a docker client"
  [prj]
  (let [uri  (uprj/get-env prj [::docker-host] "unix:///var/run/docker.sock")
        auth (uprj/get-env prj [::registry-auth])]
    (cond-> (DefaultDockerClient/builder)
            true         (.uri uri)
            (some? auth) (.registryAuth (make-registry-auth auth))
            true         (.build))))

(defn- pull-image
  "Pull a docker image"
  [prj artifact-descriptor]
  (let [image     (:image artifact-descriptor)
        client    (docker-client prj)]
    (e/try-either (.pull client image))))
