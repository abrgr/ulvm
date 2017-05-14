(defproject ulvm "0.1.0-SNAPSHOT"
 :description "Universe Level VM - the language of the system"
 :license {:name "MIT"
           :url "https://github.com/abrgr/ulvm/blob/master/LICENSE"}
 :dependencies [[org.clojure/clojure "1.8.0"]
                [clojure-future-spec "1.9.0-alpha14"]
                [org.clojure/test.check "0.9.0"]]
 :plugins [[lein-cljfmt "0.5.6"]]
 :main ^:skip-aot ulvm.cli
 :target-path "target/%s"
 :profiles {:uberjar {:aot :all}})
