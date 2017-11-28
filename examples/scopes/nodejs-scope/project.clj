(defproject ulvm-nodejs-scope "0.1.0-SNAPSHOT"
 :description "NodeJS Scope"
 :license {:name "MIT"
           :url "https://github.com/abrgr/ulvm/blob/master/LICENSE"}
 :dependencies [[org.clojure/clojure "1.8.0"]
                [clojure-future-spec "1.9.0-alpha14"]
                [org.clojure/test.check "0.9.0"]
                [http-kit "2.2.0"]
                [compojure "1.6.0"]
                [fogus/ring-edn "0.3.0"]
                [funcool/cats "2.1.0"]
                [org.clojure/data.json "0.2.6"]
                [amazonica "0.3.114"]]
 :main ^:skip-aot ulvm.scopes.nodejs
 :target-path "target/%s"
 :profiles {:uberjar {:aot :all}})
