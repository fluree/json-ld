(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.fluree/json-ld)
(def version "1.0.0")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn file-exists?
  [file]
  (-> file b/resolve-path .exists))

(defn clean
  "Delete the build target directory"
  [_]
  (println "\nCleaning target...")
  (b/delete {:path "target"}))

(defn jar
  "Build the JAR"
  [_]
  (clean nil)
  (println "\nBuilding JAR" jar-file "...")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/fluree/json-ld"
                      :connection "scm:git:git://github.com/fluree/json-ld.git"
                      :developerConnection "scm:git:ssh://git@github.com/fluree/json-ld.git"
                      :tag (str "v" version)}
                :pom-data [[:description "A JSON-LD library for Clojure and ClojureScript"]
                          [:url "https://github.com/fluree/json-ld"]
                          [:licenses
                           [:license
                            [:name "Eclipse Public License 1.0"]
                            [:url "https://opensource.org/licenses/EPL-1.0"]]]]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println "JAR built successfully!"))

(defn install
  "Install the JAR locally"
  [_]
  (if (file-exists? jar-file)
    (do
      (println "\nInstalling" jar-file "to local Maven repository...")
      (b/install {:basis @basis
                  :lib lib
                  :version version
                  :jar-file jar-file
                  :class-dir class-dir}))
    (do
      (println "JAR file not found. Building...")
      (jar nil)
      (install nil))))

(defn deploy
  "Deploy the JAR to Clojars"
  [_]
  (if (file-exists? jar-file)
    (do
      (println "\nDeploying" jar-file "to Clojars...")
      (dd/deploy {:installer :remote
                  :artifact (b/resolve-path jar-file)
                  :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
    (do
      (println "JAR file not found. Building...")
      (jar nil)
      (deploy nil))))