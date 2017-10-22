(defproject ulvm "0.1.0-SNAPSHOT"
 :description "Universe Level VM - the language of the system"
 :license {:name "MIT"
           :url "https://github.com/abrgr/ulvm/blob/master/LICENSE"}
 :dependencies [[org.clojure/clojure "1.8.0"]
                [clojure-future-spec "1.9.0-alpha14"]
                [org.clojure/test.check "0.9.0"]
                [com.spotify/docker-client "8.7.3"]
                [http-kit "2.2.0"]
                [funcool/cats "2.1.0"]
                [org.clojure/tools.cli "0.3.5"]
                [org.clojure/core.logic "0.8.11"]]
 :plugins [[lein-cljfmt "0.5.6"]]
 :main ^:skip-aot ulvm.cli
 :target-path "target/%s"
 :profiles {:uberjar {:aot :all}})
