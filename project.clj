(defproject pollock "0.1.0-SNAPSHOT"
  :description "Jackson Pollock generator"
  :url "https://github.com/tombooth/pollock"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]
                 [cheshire "5.2.0"]
                 [docopt "0.6.1"]
                 [quil "1.6.0"]]
  :main pollock.entry)
