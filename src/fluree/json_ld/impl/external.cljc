(ns fluree.json-ld.impl.external
  (:require [fluree.json-ld.impl.iri :as iri]
            #_{:clj-kondo/ignore [:unused-namespace]} ; util is used inside inline-files and inline-json macros
            [fluree.json-ld.impl.util :as util]
            [clojure.string :as str])
  (:refer-clojure :exclude [read])
  #?(:cljs (:require-macros [fluree.json-ld.impl.external :refer [inline-files inline-json]])))

#?(:clj (set! *warn-on-reflection* true))

(def vocab->file {"https://schema.org/"                        "org.schema.edn"
                  "http://schema.org/"                         "org.schema.edn"
                  "http://www.w3.org/2002/07/owl#"             "owl.edn"
                  "http://www.w3.org/2000/01/rdf-schema#"      "rdfs.edn"
                  "http://www.w3.org/2004/02/skos/core#"       "skos.edn"
                  "http://purl.org/dc/terms/"                  "org.purl.dc.terms.edn"
                  "https://w3id.org/openbadges#"               "org.w3id.openbadges.edn"
                  "https://purl.imsglobal.org/spec/clr/vocab#" "org.imsglobal.spec.clr.vocab.edn"})

(def context->file {"https://ns.flur.ee/ledger/v1"
                    {:source "contexts/fluree/ledger/v1.jsonld"
                     :parsed "contexts/fluree/ledger/v1.edn"}

                    "http://www.w3.org/ns/shacl"
                    {:source "contexts/org/w3/www/ns/shacl/v1.jsonld"
                     :parsed "contexts/org/w3/www/ns/shacl/v1.edn"}

                    "https://purl.imsglobal.org/spec/clr/v1p0/context/clr_v1p0.jsonld"
                    {:source "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.jsonld"
                     :parsed "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.edn"}

                    "https://www.w3.org/2018/credentials/v1"
                    {:source "contexts/org/w3/www/2018/credentials/v1.jsonld"
                     :parsed "contexts/org/w3/www/2018/credentials/v1.edn"}

                    "https://www.w3.org/ns/did/v1"
                    {:source "contexts/org/w3/www/ns/did/v1.jsonld"
                     :parsed "contexts/org/w3/www/ns/did/v1.edn"}

                    "https://w3id.org/did/v1"
                    {:source "contexts/org/w3/www/ns/did/v1.jsonld"
                     :parsed "contexts/org/w3/www/ns/did/v1.edn"}

                    "https://w3id.org/security/v1"
                    {:source "contexts/org/w3id/security/v1.jsonld"
                     :parsed "contexts/org/w3id/security/v1.edn"}

                    "https://w3id.org/security/v2"
                    {:source "contexts/org/w3id/security/v2.jsonld"
                     :parsed "contexts/org/w3id/security/v2.edn"}

                    "https://schema.org"
                    {:source "contexts/org/schema/latest.jsonld"
                     :parsed "contexts/org/schema/latest.edn"}

                    "https://geojson.org/geojson-ld/geojson-context.jsonld"
                    {:source "contexts/org/geojson/geojson-ld/geojson-context.jsonld"
                     :parsed "contexts/org/geojson/geojson-ld/geojson-context.edn"}})

;; set of external context URLs that are available pre-parsed.
(def external-contexts (set (keys context->file)))

(def vocabs (->> vocab->file keys (sort-by count) reverse))

#?(:clj
   (defmacro inline-files
     "The classpath doesn't exist in javascript, so we need to inline all of our resources at
  compile time so they are available to js applications at runtime."
     []
     (let [loaded-contexts (reduce (fn [l [_ {:keys [parsed]}]]
                                     (assoc l parsed (util/read-resource parsed)))
                                   {}
                                   context->file)
           loaded (reduce (fn [l [k file]]
                            (assoc l k (util/read-resource file)))
                          loaded-contexts
                          vocab->file)]
       loaded)))

#?(:clj
   (defmacro inline-json
     "The classpath doesn't exist in javascript, so we need to inline all of our resources at
  compile time so they are available to js applications at runtime."
     []
     (reduce (fn [l [url {:keys [source]}]]
               (assoc l url (util/slurp-resource source)))
             {}
             context->file)))

#?(:cljs
   (def inlined-files (inline-files)))
#?(:cljs
   (def inlined-contexts (inline-json)))

#?(:cljs
   (defn read-inlined-resource
     [k]
     (get inlined-files k)))

(defn vocab
  "Loads an entire vocabulary file, i.e. https://schema.org"
  [iri]
  (-> (get vocab->file (iri/add-trailing-slash iri))
      #?(:clj util/read-resource
         :cljs read-inlined-resource)))


(defn iri
  "Loads a supported external context and returns parsed (edn) format.

  Currently will not execute an http request to load, only pre-parsed vocabularies
  that exist in the resources folder work.

  Only supported on CLJ"
  [iri]
  (some #(when (str/starts-with? iri %)
           (-> (vocab %)
               (get iri)))
        vocabs))

(defn context
  "JSON-ld @context can be an external .jsonld file, for which we have some locally.
  Not external HTTP requests happens from this library, however anyone can implement that independently.

  Returns nil if the context requested does not exist."
  [url]
  (some-> (get context->file url)
          :parsed
          #?(:clj util/read-resource
             :cljs read-inlined-resource)))
