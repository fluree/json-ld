(ns user
  (:require [clojure.java.io :as io]
            [clojure.tools.namespace.repl :as tn :refer [refresh refresh-all]]
            [fluree.json-ld :as json-ld]
            [fluree.json-ld.impl.external :as external]
            [fluree.json-ld.impl.expand :as expand]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
  (:import (clojure.lang ExceptionInfo)))

(defn parse-context-file
  [{:keys [source dest]}]
  (let [json (-> source io/resource slurp)]
    (when-not json
      (throw (ex-info (str "Context unable to be loaded from file: " source ".")
                      {:status 400 :error :json-ld/invalid-context})))
    (let [parsed-context (-> json
                             cheshire/parse-string
                             json-ld/parse-context)]
      (spit (io/file "resources" dest)
            (with-out-str (pprint/pprint parsed-context))))))

(defn parse-context
  "Parses a single context and saves to corresponding .edn file."
  [context]
  (let [{:keys [source] :as files} (get external/context->file context)]
    (try
      (parse-context-file files)
      (catch ExceptionInfo _
        (throw (ex-info (str "Unable to load context " context " from file: " source ".")
                        {:status 400, :error :json-ld/invalid-context}))))))


(defn re-parse-all-contexts
  "Re-parses and saves all external context files"
  []
  (let [externals (keys external/context->file)]
    (doseq [context externals]
      (println "Processing: " context)
      (parse-context context))))


(comment

  (json-ld/expand {"@context"            {"gist" "https://ontologies.semanticarts.com/gist/",
                                          "owl" "http://www.w3.org/2002/07/owl#",
                                          "skos" "http://www.w3.org/2004/02/skos/core#",
                                          "xsd" "http://www.w3.org/2001/XMLSchema#"},
                   "@id"                 "gist:CoherentUnit",
                   "skos:scopeNote"      [{"@type" "xsd:string", "@value" "Coherent unit is the physics term for this, informally you might think of it as the standard unit for a given dimension."}
                                          {"@type" "xsd:string", "@value" "In principle, the CoherentUnit for a ProductUnit or RatioUnit can be inferred by recursively decomposing the products and ratios into their respective CoherentUnits, bottoming out in SimpleUnits"}],
                   "@type"               "owl:Class"})

  (json-ld/expand {"@context"    {"@base"       "https://base.com/base/iri"
                                  "@vocab"      "https://vocab.com/vocab/iri/"
                                  "iriProperty" {"@type" "@id"}}
                   "@id"         "#joebob",
                   "@type"       "Joey"
                   "name"        "Joe Bob"
                   "iriProperty" "#a-relative-id"})

  (json-ld/expand {"@context" ["https://www.w3.org/2018/credentials/v1" "https://flur.ee/ns/block"],

                   "credentialSubject" {"message" "Another commit"}
                   "type" ["VerifiableCredential"]})

  (json-ld/expand {"@context" ["https://www.w3.org/2018/credentials/v1" "https://flur.ee/ns/block"],
                   "type" ["VerifiableCredential"]
                   "credentialSubject" {"message" "Another commit"},
                   "id" "blah",
                   "issuanceDate" "2021-12-26T10:55:09.579350Z",
                   "issuer" "did:fluree:TfCzWTrXqF16hvKGjcYiLxRoYJ1B8a6UMH6",})


  (json-ld/parse-context "https://www.w3.org/2018/credentials/v1")

  (parse-context "https://ns.flur.ee/ledger/v1")

  (parse-context "https://geojson.org/geojson-ld/geojson-context.jsonld")

  (re-parse-all-contexts)

  (json-ld/parse-context ["https://flur.ee/ns/block"
                          {"schema" "http://schema.org/"}]))
