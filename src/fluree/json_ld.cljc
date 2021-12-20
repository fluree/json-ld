(ns fluree.json-ld
  (:require [fluree.json-ld.context :as context]
            [fluree.json-ld.compact :as compact]
            [fluree.json-ld.expand :as expand]
            [fluree.json-ld.normalize :as normalize]
            #?(:clj [fluree.json-ld.external :as external])))

#?(:clj (set! *warn-on-reflection* true))

(defn parse-context
  "Parses a JSON-LD context and returns a Clojure map (or error if context invalid).

  If a base-context is provided, merges new context into base context. base-context
  must already be a parsed context.

  externals, if provided, is a set of external context URLs that are safe to load. Currently
  will only load external contexts that are pre-parsed and saved locally with this library,
  the default list of which is at fluree.json-ld.external/external-contexts"
  ([context] (context/parse {} external/external-contexts context))
  ([base-context context]
   (context/parse base-context external/external-contexts context))
  ([base-context externals context]
   (context/parse base-context externals context)))


(defn external-vocab
  "Loads a supported external vocabulary for a specific iri, which should be
  a class or predicate.

  The vocab will include information beyond a context mapping, i.e. rdfs:subClassOf."
  [iri]
  (external/vocab iri))

(defn external-iri
  "Loads a supported external vocabulary for a specific iri, which should be
  a class or predicate.

  The vocab will include information beyond a context mapping, i.e. rdfs:subClassOf."
  [iri]
  (external/iri iri))


(defn external-context
  "Loads a pre-fetched, parsed context based on URL that may be used as a @context value.
  Note this is not a vocabulary, but just a context mapping. It may end up using many different
  vocabularies as part of its mapping..

  Returns nil if context not pre-fetched."
  [url]
  (external/context url))


(defn compact
  "Returns compacted iri when provided parsed context."
  [iri parsed-context-or-fn]
  (compact/compact iri parsed-context-or-fn))


(defn compact-fn
  "Returns compacting fn based on the provided parsed context
  that takes a single argument string IRI and returns compacted IRI.

  If IRI cannot be compacted, returns original IRI.

  Optionally, a second param, used-atom (a Clojure atom), can be supplied which captures all
  context items that were used. This is useful when a large context is present which only a
  small subset will be used, and you want to know which subset were used."
  ([parsed-context]
   (compact/compact-fn parsed-context nil))
  ([parsed-context used-atom]
   (compact/compact-fn parsed-context used-atom)))


(defn expand-iri
  "Expands a compacted iri string to full iri.

  If vocab? is true, it will expand based on the value being a property/class (@type)
  and utilize the context's :vocab value, if defined. If vocab? is falsey, will
  treat compact-iri as an @id, and will utilize the context's :base value, if defined.

  If the iri is not compacted, returns original iri string."
  ([compact-iri parsed-context]
   (expand/iri compact-iri parsed-context true))
  ([compact-iri parsed-context vocab?]
   (expand/iri compact-iri parsed-context vocab?)))


(defn expand
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([node-map]
   (expand/node node-map {}))
  ([node-map parsed-context]
   (expand/node node-map parsed-context)))


(defn details
  "Like expand, but returns two-tuple of expanded iri followed by
  a map of any context settings for that iri.

  If vocab? is true, it will expand based on the value being a property/class (@type)
  and utilize the context's :vocab value, if defined. If vocab? is falsey, will
  treat compact-iri as an @id, and will utilize the context's :base value, if defined.

  If no match exists, returns original compact-iri as first element
  and nil for second."
  ([compact-iri parsed-context]
   (expand/details compact-iri parsed-context true))
  ([compact-iri parsed-context vocab?]
   (expand/details compact-iri parsed-context vocab?)))


(defn json-ld?
  "Returns true if the provided document looks like json-ld."
  [x]
  (boolean
    (or
      (get x "@graph")
      (get-in x [0 "@context"])
      (get-in x [0 "@id"]))))


(defn normalize-data
  "Normalizes a Clojure data structure into a string based on provided options which
  include
  :algorithm - :basic or :URDNA2015 (not yet supported)
  :format - what format to put data into :application/json (default) :application/n-quads (not yet supported)

  Used to get a consistent format/hash for data regardless of input."
  ([data] (normalize/normalize data))
  ([data opts]
   (normalize/normalize data opts)))
