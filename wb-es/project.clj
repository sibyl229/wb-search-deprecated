(defproject wb-es "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.7.0"
  :sign-releases false
  :dependencies
  [[cheshire "5.7.0"]
   [clj-http "2.3.0"]
   [org.clojure/core.async "0.3.442"]
   [environ "1.1.0"]
   [mount "0.1.11"]
   [org.clojure/clojure "1.8.0"]

   ;; the following dependecies are only needed for web
   ]
  :source-paths ["src"]
  :plugins [[lein-environ "1.1.0"]
            [lein-pprint "1.1.1"]]
  :main ^:skip-aot wb-es.core
  :resource-paths ["resources"]
  :target-path "target/%s"
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :license "GPLv2"
  :jvm-opts ["-Xmx2G"
             ;; same GC options as the transactor,
             ;; should minimize long pauses.
             "-XX:+UseG1GC" "-XX:MaxGCPauseMillis=50"
             "-Ddatomic.objectCacheMax=1000000000"
             "-Ddatomic.txTimeoutMsec=1000000"]
  :profiles
  {:datomic-free
   {:dependencies [[com.datomic/datomic-free "0.9.5561.50"
                    :exclusions [joda-time]]]}
   :datomic-pro
   {:dependencies [[com.datomic/datomic-pro "0.9.5561.50"
                    :exclusions [joda-time]]]}
   :ddb
   {:dependencies
    [[com.amazonaws/aws-java-sdk-dynamodb "1.11.82"
      :exclusions [joda-time]]]}
   :indexer [:datomic-pro :ddb]
   :web [:datomic-free
         {:main ^:skip-aot wb-es.web.index
          :uberjar-name "wb-es-web-%s-standalone.jar"
          :dependencies [[compojure "1.6.0"]
                         [ring/ring-defaults "0.3.0"]
                         [ring/ring-core "1.6.2"]
                         [ring/ring-json "0.4.0"]]
          :ring {:handler wb-es.web.index/handler}}]
   :web-dev {:dependencies [[ring/ring-devel "1.5.1"]]
             :plugins [[lein-ring "0.12.0"]]}
   :dev [:indexer
         :web
         :web-dev
         {:aliases
          {"code-qa"
           ["do"
            ["eastwood"]
            "test"]}
          :source-paths ["dev"]
          :env
          {:wb-db-uri "datomic:ddb://us-east-1/WS260/wormbase"
           :swagger-validator-url "http://localhost:8002"}
          :plugins
          [[jonase/eastwood "0.2.4"
            :exclusions [org.clojure/clojure]]
           [lein-ancient "0.6.8"]
           [lein-bikeshed "0.3.0"]
           [lein-ns-dep-graph "0.1.0-SNAPSHOT"]
           [venantius/yagni "0.1.4"]
           [com.jakemccrary/lein-test-refresh "0.17.0"]]}]
   :uberjar {:aot :all}
   :test
   {:resource-paths ["test/resources"]}}
  :aliases {"test" ["with-profile" "+indexer,+web" "test"]
            "test-refresh" ["with-profile" "+indexer,+web" "test-refresh"]}
  :repl-options {:init (do
                         (set! *print-length* 10)
                         (use 'wb-es.env)
                         (use 'wb-es.datomic.db)
                         (require '[datomic.api :as d])
                         (mount.core/start))})
