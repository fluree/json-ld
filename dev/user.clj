(ns user
  (:require [clojure.tools.namespace.repl :as tn :refer [refresh refresh-all]]
            [fluree.json-ld :as json-ld]
            [fluree.json-ld.impl.external :as external]
            [fluree.json-ld.impl.expand :as expand]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]))


(defn parse-context
  "Parses a single context and saves to corresponding .edn file."
  [context]
  (let [{:keys [source parsed]} (get external/context->file context)
        json (-> source io/resource slurp)]
    (when-not json
      (throw (ex-info (str "Context " context " unable to be loaded from file: " source ".")
                      {:status 400 :error :json-ld/invalid-context})))
    (->> json
         cheshire/parse-string
         json-ld/parse-context
         pprint/pprint
         with-out-str
         (spit (io/file "resources" parsed)))))


(defn re-parse-all-contexts
  "Re-parses and saves all external context files"
  []
  (let [externals (keys external/context->file)]
    (doseq [context externals]
      (println "Processing: " context)
      (parse-context context))))


(comment

  (parse-context "https://flur.ee/ns/block")

  (parse-context "https://geojson.org/geojson-ld/geojson-context.jsonld")

  (re-parse-all-contexts)

  (json-ld/parse-context ["https://flur.ee/ns/block"
                          {"schema" "http://schema.org/"}])

  )
