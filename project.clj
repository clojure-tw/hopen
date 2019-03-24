(defproject hopen/hopen "0.1.0-SNAPSHOT"
  :description "A simple, modern, flexible and portable template engine for Clojure environments."
  :url "https://github.com/clojure-tw/hopen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src"]
  :resource-paths ["resources"]
  :target-path "target/%s"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.520"]]
  :plugins [[lein-doo "0.1.10"]]
  :cljsbuild
  {:builds [{:id "cljs-test"
             :source-paths ["src" "test"]
             :compiler {:output-to "resources/public/js/index.js"
                        :main hopen.runner
                        :optimizations :none
                        :pretty-print true
                        :target :nodejs}}]})
