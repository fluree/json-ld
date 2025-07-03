#!/usr/bin/env clojure

(ns scripts.parse-contexts
  (:require [clojure.java.io :as io]
            [fluree.json-ld :as json-ld]
            [fluree.json-ld.impl.external :as external]
            [cheshire.core :as cheshire]
            [clojure.pprint :as pprint])
  (:import (clojure.lang ExceptionInfo)))

(defn parse-context-file
  [{:keys [source parsed]}]
  (let [json (-> source io/resource slurp)]
    (when-not json
      (throw (ex-info (str "Context unable to be loaded from file: " source ".")
                      {:status 400 :error :json-ld/invalid-context})))
    (let [parsed-context (-> json
                             cheshire/parse-string
                             json-ld/parse-context)]
      (spit (io/file "resources" parsed)
            (with-out-str (pprint/pprint parsed-context))))))

(defn parse-context
  "Parses a single context and saves to corresponding .edn file."
  [context]
  (let [{:keys [source] :as files} (get external/context->file context)]
    (try
      (parse-context-file files)
      (println "✓ Parsed:" context "from" source)
      (catch ExceptionInfo e
        (println "✗ Failed to parse:" context "from" source)
        (println "  Error:" (.getMessage e))
        (throw e)))))

(defn re-parse-all-contexts
  "Re-parses and saves all external context files"
  []
  (let [externals (keys external/context->file)
        total (count externals)]
    (println (str "\nParsing " total " context files...\n"))
    (doseq [[idx context] (map-indexed vector externals)]
      (print (str "[" (inc idx) "/" total "] Processing: " context "... "))
      (flush)
      (try
        (parse-context context)
        (catch Exception _
          (println "\n\nError parsing contexts. Aborting.")
          (System/exit 1))))
    (println (str "\n✓ Successfully parsed all " total " context files!"))))

;; Main entry point
(defn -main [& _args]
  (try
    (re-parse-all-contexts)
    (System/exit 0)
    (catch Exception e
      (println "Fatal error:" (.getMessage e))
      (System/exit 1))))

;; Run the main function
(-main)