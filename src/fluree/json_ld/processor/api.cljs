(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require ["jsonld/dist/jsonld.esm.min.js" :as jldjs]
            [fluree.json-ld.impl.external :as external]))

(defn pluggable-loader
  "Takes a document-loader, which takes a url and options and returns a json string
  context document (must have an \"@context\" key with a context as its value).

  document-loader: [url options] => json-string
  "
  [document-loader]
  (fn [url options]
    (js/Promise.
     (fn [resolve reject]
       (try
         (let [json-string (document-loader url options)]
           (resolve #js{"contextUrl" nil, "document" json-string, "documentUrl" url}))
         (catch js/Error _
           (reject (throw #js{"name" "jsonld.LoadDocumentError"
                              "message" (str "Unable to load static context: " url)
                              "details" {"code" "loading remote context failed"
                                         "url" url}}))))))))

(defn static-loader
  [url _]
  (get external/inlined-contexts url))

(defn compact
  ([json-ld context]
   (compact json-ld context {:document-loader static-loader}))
  ([json-ld context opts]
   (-> json-ld
       (clj->js)
       (jldjs/compact (clj->js context) #js{"documentLoader" (pluggable-loader (:document-loader opts))})
       (.then (fn [result] (js->clj result))))))

(defn expand
  ([json-ld]
   (expand json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> json-ld
       (clj->js)
       (jldjs/expand #js{"documentLoader" (pluggable-loader (:document-loader opts))})
       (.then (fn [result] (js->clj result))))))

(defn flatten
  "Flattens JSON-LD documents into a single array with node references.
  
  NOTE: This function may not work reliably in ClojureScript due to 
  compatibility issues with the underlying JavaScript JSON-LD library.
  Consider using the Clojure version for flatten operations."
  ([json-ld]
   (flatten json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> json-ld
       (clj->js)
       (jldjs/flatten #js{"documentLoader" (pluggable-loader (:document-loader opts))})
       (.then (fn [result] (js->clj result))))))

(defn from-rdf
  ([n-quads]
   (from-rdf n-quads {:document-loader static-loader}))
  ([n-quads opts]
   (-> n-quads
       (clj->js)
       (jldjs/fromRDF #js{"format" "application/n-quads"
                          "documentLoader" (pluggable-loader (:document-loader opts))})
       (.then (fn [result] (js->clj result))))))

(defn to-rdf
  ([json-ld]
   (to-rdf json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> json-ld
       (clj->js)
       (jldjs/expand #js{"documentLoader" (pluggable-loader (:document-loader opts))})
       (.then (fn [expanded] (jldjs/toRDF expanded #js{"format" "application/n-quads"}))))))

(defn canonize
  ([json-ld]
   (canonize json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> (to-rdf json-ld opts)
       (.then (fn [rdf]
                (jldjs/canonize rdf #js {"algorithm" "URDNA2015" "inputFormat" "application/n-quads"}))))))
