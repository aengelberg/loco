(defproject loco "0.2.1-SNAPSHOT"
  :description "Constraint Programming for Clojure"
  :url "http://github.com/aengelberg/loco"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["choco.repos" "http://www.emn.fr/z-info/choco-repo/mvn/repository/"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [choco/choco-solver "3.1.0"]])
