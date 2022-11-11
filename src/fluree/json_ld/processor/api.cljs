(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require ["jsonld" :as jldjs]
            [fluree.json-ld.impl.external :as external]))

(defn compact
  [json-ld context]
  (-> json-ld
      (clj->js)
      (jldjs/compact (clj->js context) #js{"contextUrl" nil, "document" context, "documentUrl" url})
      (.then (fn [result] (js->clj result)))))

(defn static-loader
  [url options]
  (js/Promise.
    (fn [resolve reject]
      (if-let [context (get external/inlined-contexts url)]
        (resolve #js{"contextUrl" nil, "document" context, "documentUrl" url})
        (reject (throw #js{"name" "jsonld.LoadDocumentError"
                           "message" (str "Unable to load static context: " url)
                           "details" {"code" "loading remote context failed"
                                      "url" url}}))))))

(defn expand
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/expand #js{"documentLoader" static-loader})
      (.then (fn [result] (js->clj result)))))

(defn flatten
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/flatten #js{"contextUrl" nil, "document" context, "documentUrl" url})
      (.then (fn [result] (js->clj result)))))

(defn from-rdf
  [n-quads]
  (-> n-quads
      (clj->js)
      (jldjs/fromRDF #js{"format" "application/n-quads"} #js{"contextUrl" nil, "document" context, "documentUrl" url})
      (.then (fn [result] (js->clj result)))))

(defn to-rdf
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/toRDF #js{"format" "application/n-quads"})))

(defn canonize
  [json-ld]
  (-> (to-rdf json-ld)
      (.then (fn [rdf]
               (jldjs/canonize rdf #js {"algorithm" "URDNA2015" "inputFormat" "application/n-quads"})))))
