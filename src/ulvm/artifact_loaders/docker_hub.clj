(ns ^{:author "Adam Berger"} ulvm.artifact-loaders.docker-hub
  "ULVM builtin Docker Hub loader"
  (:require [org.httpkit.client :as http-kit]
            [ulvm.mod-loaders :as l]
            [ulvm.project :as uprj]
            [ulvm.runnable-envs :as renv]
            [cats.monad.either :as e])
  (:import java.util.Base64))

(declare pull-image)

(defmethod renv/builtin-load-artifact :ulvm.artifact-loaders/docker-hub
  [prj _ desc]
  (pull-image prj desc))

(defn- to-b64
  "Return string representation of the base64 representation of s or nil if s is nil"
  [s]
  (if s
    (.encodeToString (java.util.Base64/getEncoder) (.getBytes s))))

(defn- pull-image
  "Pull a docker image"
  [prj artifact-descriptor]
  (let [scheme-and-host (uprj/get-env prj [::docker-host] "unix:/var/run/docker.sock")
        url (-> (java.net.URI. scheme-and-host)
                (.resolve "/images/create")
                (.toString))
        image (::image artifact-descriptor)
        image-tag (::image-tag artifact-descriptor)
        registry-auth (::registry-auth artifact-descriptor)
        proxy-url (::proxy-url artifact-descriptor)
        headers (merge {}
                       (when registry-auth {"X-Registry-Auth" (to-b64 registry-auth)}))
        opts (merge {:query-params {:fromImage image
                                    :tag image-tag}
                     :headers headers}
                    (when proxy-url {:proxy-url proxy-url}))]
    (http-kit/post url opts
                   (fn [{:keys [status body error]}]
                     (cond
                       (some? error) (e/left {::error error})
                       (not= status 200) (e/left {::error (str status ": " body)})
                       :default (e/right body))))))
