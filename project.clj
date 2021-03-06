(defproject clj-LJ "0.1.0-SNAPSHOT"
  :description "an example of using LJ from clojure"
  :url "https://github.com/vadali/clj-LJ"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [junit/junit "4.10"]
                 [org.scala-lang/scala-library "2.9.1"]]

  :plugins [[lein-scalac "0.1.0"]]
  :scala-source-path "scala"
  :main clj-LJ.core

  :prep-tasks [ "javac" "scalac"]
  :java-source-paths ["java/src"]
)
