(ns ^{:author "Adam Berger"} ulvm.env
  "Environment"
  (:require [ulvm.reader :as r]
            [ulvm.env-keypaths :as k]))

(defn- get-env
  [dir]
  (-> [dir ".ulvm-env"]
      (r/with-file-stream r/edn-seq)
      first))

(defn resolve-initial-env
  "The initial env is composed of the following, in order of decreasing
   precedence:
   1. Command-line environment
   2. .ulvm-env file in the project directory
   3. .ulvm-env file in the home directory"
  [cmd-line-env project-dir]
  (merge (assoc-in {} (k/project-root) project-dir)
         (get-env (System/getProperty "user.home"))
         (get-env project-dir)
         cmd-line-env))
