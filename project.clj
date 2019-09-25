(defproject test1 "0.1.0-SNAPSHOT"
  :description "Fun attempt to re-create the J-PET data processing and reconstruction scheme on the JVM with Clojure"
  :url "https://github.com/alekgajos/j-pet-standalone-analysis"
  :license {:name "Apache 2.0 License"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"],
                 [smee/binary "0.5.4"]
                 [cheshire "5.9.0"]
                 ]
  :profiles {:read-hld {:main test1.reader}
             :parse-data {:main test1.parser}
             }
  :aliases {"read" ["with-profile" "read-hld" "run"]
            "parse" ["with-profile" "parse-data" "run"]
            }
  
  )
