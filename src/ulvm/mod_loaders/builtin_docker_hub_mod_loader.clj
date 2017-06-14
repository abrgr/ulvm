(ns ^{:author "Adam Berger"} ulvm.mod-loaders.builtin-docker-hub-mod-loader
  "ULVM builtin Docker-Hub loader"
  (:require [org.httpkit.client :as http-kit]
            [ulvm.mod-loaders :as l]
            [ulvm.project :as uprj]
            [cats.monad.either :as e])
  (:import java.util.Base64))

(declare to-b64 pull-image)

(deftype BuiltinDockerHubLoader []
  uprj/ModLoader
  (-load-module [this prj module-descriptor] (pull-image prj module-descriptor)))

(defmethod uprj/make-mod-loader ::l/docker-hub
  [prj mod-loader-name mod-loader-entity]
  (uprj/set
   prj
   :mod-loaders
   mod-loader-name
   (e/right (BuiltinDockerHubLoader.))))

(defn- to-b64
  "Return string representation of the base64 representation of s or nil if s is nil"
  [s]
  (if s
    (.encodeToString (java.util.Base64/getEncoder) (.getBytes s))))

(defn- pull-image
  "Pull a docker image"
  [prj module-descriptor]
  (let [scheme-and-host (uprj/get-env prj [::docker-host])
        url (-> (java.net.URI. scheme-and-host)
                (.resolve "/images/create")
                (.toString))
        image (::image module-descriptor)
        image-tag (::image-tag module-descriptor)
        registry-auth (::registry-auth module-descriptor)
        proxy-url (::proxy-url module-descriptor)
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
