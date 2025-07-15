(ns fluree.json-ld-js
  "JavaScript-friendly wrappers for fluree.json-ld functions.
  Handles conversion between JavaScript and ClojureScript data structures."
  (:require [fluree.json-ld :as jld]))

(defn ^:export expand
  "Expands a JSON-LD document. Converts JS objects to CLJ and back."
  ([js-node-map]
   (let [clj-node-map (js->clj js-node-map :keywordize-keys false)
         result (jld/expand clj-node-map)]
     (clj->js result)))
  ([js-node-map js-parsed-context]
   (let [clj-node-map (js->clj js-node-map :keywordize-keys false)
         ; parsed-context is already CLJ from parse-context
         result (jld/expand clj-node-map js-parsed-context)]
     (clj->js result))))

(defn ^:export compact
  "Compacts an expanded IRI using the provided parsed context."
  [js-iri js-parsed-context-or-fn]
  (let [clj-iri (if (string? js-iri) js-iri (js->clj js-iri :keywordize-keys false))
        ; parsed-context is already CLJ from parse-context
        result (jld/compact clj-iri js-parsed-context-or-fn)]
    result))

(defn ^:export normalize-data
  "Normalizes JSON-LD data to a consistent string format."
  ([js-data]
   (let [clj-data (js->clj js-data :keywordize-keys true)]
     (jld/normalize-data clj-data)))
  ([js-data js-opts]
   (let [clj-data (js->clj js-data :keywordize-keys true)
         clj-opts (js->clj js-opts :keywordize-keys true)]
     (jld/normalize-data clj-data clj-opts))))

(defn ^:export parse-context
  "Parses a JSON-LD context and returns a parsed context."
  ([js-context]
   (let [clj-context (js->clj js-context :keywordize-keys false)
         result (jld/parse-context clj-context)]
     result)) ; Keep as CLJ for internal use
  ([js-base-context js-context]
   (let [clj-base-context (js->clj js-base-context :keywordize-keys false)
         clj-context (js->clj js-context :keywordize-keys false)
         result (jld/parse-context clj-base-context clj-context)]
     result))
  ([js-base-context js-externals js-context]
   (let [clj-base-context (js->clj js-base-context :keywordize-keys false)
         clj-externals (js->clj js-externals :keywordize-keys false)
         clj-context (js->clj js-context :keywordize-keys false)
         result (jld/parse-context clj-base-context clj-externals clj-context)]
     result)))

(defn ^:export expand-iri
  "Expands a compact IRI to its full form."
  ([js-compact-iri js-parsed-context]
   (let [clj-compact-iri (str js-compact-iri)
         clj-parsed-context (js->clj js-parsed-context :keywordize-keys true)
         result (jld/expand-iri clj-compact-iri clj-parsed-context)]
     result))
  ([js-compact-iri js-parsed-context js-vocab?]
   (let [clj-compact-iri (str js-compact-iri)
         clj-parsed-context (js->clj js-parsed-context :keywordize-keys true)
         result (jld/expand-iri clj-compact-iri clj-parsed-context js-vocab?)]
     result)))

(defn ^:export compact-fn
  "Returns a function that compacts expanded IRIs using the parsed context."
  ([js-parsed-context]
   (let [clj-parsed-context (js->clj js-parsed-context :keywordize-keys true)
         compact-fn (jld/compact-fn clj-parsed-context)]
     (fn [iri] (compact-fn iri))))
  ([js-parsed-context js-used-atom]
   (let [clj-parsed-context (js->clj js-parsed-context :keywordize-keys true)
         clj-used-atom (js->clj js-used-atom :keywordize-keys true)
         compact-fn (jld/compact-fn clj-parsed-context clj-used-atom)]
     (fn [iri] (compact-fn iri)))))

(defn ^:export json-ld?
  "Returns true if the document appears to be JSON-LD."
  [js-doc]
  (let [clj-doc (js->clj js-doc :keywordize-keys false)]
    (boolean
     (or
      ; Direct properties
      (get clj-doc "@context")
      (get clj-doc "@id")
      (get clj-doc "@type")
      (get clj-doc "@graph")
      ; Array format
      (jld/json-ld? clj-doc)))))

;; Export object for compatibility
(def ^:export exports
  #js {:expand        expand
       :compact       compact
       :normalizeData normalize-data
       :parseContext  parse-context
       :expandIri     expand-iri
       :compactFn     compact-fn
       :jsonLd        json-ld?})