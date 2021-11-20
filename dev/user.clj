(ns user
  (:require [clojure.tools.namespace.repl :as tn :refer [refresh refresh-all]]
            [fluree.json-ld :as json-ld]
            [fluree.json-ld.external :as external]
            [fluree.json-ld.expand :as expand]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]))


(defn re-parse-all-contexts
  "Re-parses and saves all external context files"
  []
  (let [externals (vals external/context->file)]
    (doseq [{:keys [source parsed]} externals]
      (println "Processing: " source)
      (let [source-data (-> source
                            io/resource
                            slurp
                            cheshire/parse-string
                            json-ld/parse-context)]
        (->> (pprint/pprint source-data)
             with-out-str
             (spit (io/file "resources" parsed)))))))


(comment

  (re-parse-all-contexts)


  )