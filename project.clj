(defproject hw1 "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.xml "0.0.8"]]
  :main ^:skip-aot hw1.core
  :java-source-paths ["src-java"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

