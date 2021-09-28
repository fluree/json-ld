(ns fluree.json-ld.external
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [fluree.json-ld.iri :as iri])
  (:refer-clojure :exclude [read]))

(def vocab->file {"https://schema.org/" "schema.org.edn"})

(def vocabs (->> vocab->file keys (sort-by count) reverse))


(defn read
  "Given a full IRI, finds a matching vocab file from the
  vocab->file map and reads contents, else returns nil."
  [iri]
  (some #(when (str/starts-with? iri %)
           (-> (get vocab->file %)
               io/resource
               slurp
               edn/read-string))
        vocabs))


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


(comment

  (iri "https://schema.org/commentCount")

  )

