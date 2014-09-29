(defproject hw1 "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.xml "0.0.8"]
                 [criterium "0.4.3"]
                 [swiss-arrows "1.0.0"]
                 [net.mikera/core.matrix "0.30.1"]]
  :plugins [[codox "0.8.10"]]
  :codox {:defaults {:doc/format :markdown}}
  :main ^:skip-aot hw1.core
  :java-source-paths ["src-java"]
  :target-path "target/%s"
  :omit-source true
  :profiles {:uberjar {:aot :all}})

