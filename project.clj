(defproject com.ambrosebs/dynalint "0.1.4-SNAPSHOT"
  :description "Lint your Clojure program by running them"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.19"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :profiles {:dev {:repl-options {:port 64476}}}
  :warn-on-reflection true)
