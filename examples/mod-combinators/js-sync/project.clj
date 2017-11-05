(defproject ulvm "0.1.0-SNAPSHOT"
 :description "Synchronous Javascript Module Combinator"
 :license {:name "MIT"
           :url "https://github.com/abrgr/ulvm/blob/master/LICENSE"}
 :dependencies [[org.clojure/clojure "1.8.0"]
                [clojure-future-spec "1.9.0-alpha14"]
                [org.clojure/test.check "0.9.0"]
                [http-kit "2.2.0"]
                [compojure "1.6.0"]
                [fogus/ring-edn "0.3.0"]]
 :main ^:skip-aot ulvm.mod-combinators.js.sync
 :target-path "target/%s"
 :profiles {:uberjar {:aot :all}})
