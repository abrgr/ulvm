(ns ^{:author "Adam Berger"} ulvm.re-loaders.builtin-project-file-re-loader
  "ULVM builtin project file runnable environment loader"
  (:require [ulvm.re-loaders :as l]
            [ulvm.core :as ucore]
            [ulvm.reader :as reader]
            [ulvm.project :as uprj]
            [cats.core :as m]
            [cats.monad.either :as e]
            [clojure.java.io :as io]))

(declare load-from-file)

(deftype BuiltinProjectFileRELoader []
  uprj/REnvLoader
  (-get-runnable-env-rep [this prj desc]
    (m/mlet [root (uprj/get-env prj [::ucore/project-root])]
      (e/try-either
       (load-from-file root (:path desc))))))

(defn- load-from-file
  [project-dir path]
  (reader/read-ulvm-file (io/file project-dir path)))

(defmethod uprj/make-renv-loader ::l/project-file
  [prj re-loader-name re-loader-entity]
  (uprj/set
   prj
   :renv-loaders
   re-loader-name
   (e/right (BuiltinProjectFileRELoader.))))
