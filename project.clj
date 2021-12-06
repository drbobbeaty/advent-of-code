(defproject advent-of-code "0.1.0"
  :description "Fun project to play with"
  :url "http://github.com/drbobbeaty/advent-of-code"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src"]
  :min-lein-version "2.3.4"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.memoize "0.5.8"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ;; logging with log4j
                 [org.slf4j/slf4j-log4j12 "1.7.5"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/tools.logging "0.2.6"]
                 [robert/hooke "1.3.0"]
                 ;; JSON and time libraries
                 [cheshire "5.6.3"]
                 [clojure.java-time "0.3.2"]
                 [pandect "0.5.4"]]
  :jvm-opts ["-Xmx2g" "-Xss500m"])
