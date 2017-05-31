(ns ulvm.loaders.docker-hub-loader
  "ULVM builtin Docker-Hub loader"
  (:require [org.httpkit.client :as http-kit]
            [ulvm.loaders :as l])
  (:import java.util.Base64))

(declare to-b64 pull-image)

(deftype BuiltinDockerHubLoader [env]
  l/Loader
  (-launch [this] {:l/success true})
  (-load-module [this module-descriptor] (pull-image env module-descriptor))
  (-stop [this] {:l/success true}))

(defmethod l/make-loader :l/docker-hub
  [_ _ env]
  (BuiltinDockerHubLoader. env))

(defn- to-b64
  "Return string representation of the base64 representation of s or nil if s is nil"
  [s]
  (if s
    (.encodeToString (java.util.Base64/getEncoder) (.getBytes s))))

(defn- pull-image
  "Pull a docker image"
  [env module-descriptor]
  (let [scheme-and-host (get env ::docker-host)
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
                       (some? error) {:l/err error}
                       (not= status 200) {:l/err (str status ": " body)}
                       :default {:l/success body})))))
