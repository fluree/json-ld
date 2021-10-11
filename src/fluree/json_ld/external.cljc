(ns fluree.json-ld.external
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [fluree.json-ld.iri :as iri])
  (:refer-clojure :exclude [read]))

(def vocab->file {"https://schema.org/"            "org.schema.edn"
                  "http://www.w3.org/2002/07/owl#" "owl.edn"
                  "http://purl.org/dc/terms/"      "org.purl.dc.terms.edn"})

(def context->file {"https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld"
                    "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.edn"})

(def vocabs (->> vocab->file keys (sort-by count) reverse))


(defn read
  "Given a full IRI, finds a matching vocab file from the
  vocab->file map and reads contents, else returns nil."
  [iri]
  (try
    (some #(when (str/starts-with? iri %)
             (-> (get vocab->file %)
                 io/resource
                 slurp
                 edn/read-string))
          vocabs)
    (catch Exception e
      (throw (ex-info
               (str "Invalid IRI, unable to read vocabulary: " iri)
               {:status 400 :error :json-ld/invalid-iri}
               e)))))


(defn vocab
  "Loads an entire vocabulary file, i.e. https://schema.org"
  [iri]
  (-> iri
      iri/add-trailing-slash
      read))


(defn iri
  "Loads a supported external context and returns parsed (edn) format.
  
  Currently will not execute an http request to load, only pre-parsed vocabularies
  that exist in the resources folder work.
  
  Only supported on CLJ"
  [iri]
  (some-> (read iri)
          (get iri)))


(defn context
  "JSON-ld @context can be an external .jsonld file, for which we have some locally.
  Not external HTTP requests happens from this library, however anyone can implement that independently.

  Returns nil if the context requested does not exist."
  [url]
  (some-> (get context->file url)
          io/resource
          slurp
          edn/read-string))


(comment

  (iri "https://schema.org/commentCount")

  (iri "http://www.w3.org/2002/07/owl#ObjectProperty")

  (context "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld")
  (get context->file "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld")

  (fluree.json-ld.context/parse (context "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld"))

  )

