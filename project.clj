(defproject my-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.10.1"]
                 [ring/ring-json "0.4.0" :exclusions [cheshire ring/ring-core]]
                 [enlive "1.1.6" :exclusions [org.jsoup/jsoup]]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.jsoup/jsoup "1.7.3"]
                 [clj-http "3.10.1"]
				 [clj-webdriver "0.6.0"]
             ]
	:main ^:skip-aot my-app.core
  :target-path "target/%s"
  :repl-options {
             :timeout 120000
             }
  :profiles {:uberjar {:aot :all}})
