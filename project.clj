(defproject lein-lock "0.1.1"
  :description "A Leiningen plugin to check transitive dependencies against a dependencies.lock file to ensure repeatable builds."
  :url "https://github.com/SparkFund/lein-lock"
  :dependencies [[pandect "0.5.1"]]
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :scm {:name "git"
        :url "https://github.com/SparkFund/lein-lock"}
  :eval-in-leiningen true)
