(ns fluree.json-ld.external
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.util :as util]
            [clojure.string :as str])
  (:refer-clojure :exclude [read]))

#?(:clj (set! *warn-on-reflection* true))

(def vocab->file {"https://schema.org/"                        "org.schema.edn"
                  "http://schema.org/"                         "org.schema.edn"
                  "http://www.w3.org/2002/07/owl#"             "owl.edn"
                  "http://purl.org/dc/terms/"                  "org.purl.dc.terms.edn"
                  "https://w3id.org/openbadges#"               "org.w3id.openbadges.edn"
                  "https://purl.imsglobal.org/spec/clr/vocab#" "org.imsglobal.spec.clr.vocab.edn"})

(def context->file {"https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld"
                    "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.edn"

                    "https://www.w3.org/2018/credentials/v1"
                    "contexts/org/w3/www/2018/credentials/v1.edn"})

(def vocabs (->> vocab->file keys (sort-by count) reverse))


(defn vocab
  "Loads an entire vocabulary file, i.e. https://schema.org"
  [iri]
  #?(:cljs (throw (ex-info (str "Loading external vocabularies is not yet supported in Javascript.")
                           {:status 400 :error :json-ld/external-vocab}))
     :clj  (->> iri
                iri/add-trailing-slash
                (get vocab->file)
                util/read-resource)))


(defn iri
  "Loads a supported external context and returns parsed (edn) format.
  
  Currently will not execute an http request to load, only pre-parsed vocabularies
  that exist in the resources folder work.
  
  Only supported on CLJ"
  [iri]
  #?(:cljs (throw (ex-info (str "Loading external vocabularies is not yet supported in Javascript.")
                           {:status 400 :error :json-ld/external-vocab}))
     :clj  (some #(when (str/starts-with? iri %)
                    (-> (vocab %)
                        (get iri)))
                 vocabs)))




(defn context
  "JSON-ld @context can be an external .jsonld file, for which we have some locally.
  Not external HTTP requests happens from this library, however anyone can implement that independently.

  Returns nil if the context requested does not exist."
  [url]
  #?(:cljs (throw (ex-info (str "Loading external contexts is not yet supported in Javascript.")
                           {:status 400 :error :json-ld/external-context}))
     :clj  (some-> (get context->file url)
                   util/read-resource)))


(comment

  (iri "https://schema.org/Person")

  (iri "https://purl.imsglobal.org/spec/clr/vocab#dtExtensibleAchievementType")

  (context "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld")
  (get context->file "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld")

  (fluree.json-ld.context/parse (context "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld"))

  )

