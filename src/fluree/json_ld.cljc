(ns fluree.json-ld
  (:require [fluree.json-ld.impl.context :as context]
            [fluree.json-ld.impl.compact :as compact]
            [fluree.json-ld.impl.expand :as expand]
            [fluree.json-ld.impl.normalize :as normalize]
            [fluree.json-ld.impl.external :as external]))

#?(:clj (set! *warn-on-reflection* true))

(defn parse-context
  "Parses a JSON-LD context and returns a parsed context with an internal 
   representation to make repeated expansion/compaction more efficient
   for use in other API calls.
  
  Optional parameters:
  - base-context: Previously parsed context to merge into the new un-parsed context
  - externals: Set of allowed external context URLs (defaults to pre-loaded contexts)"
  ([context] (context/parse {} external/external-contexts context))
  ([base-context context]
   (context/parse base-context external/external-contexts context))
  ([base-context externals context]
   (context/parse base-context externals context)))


(defn external-vocab
  "Returns vocabulary information for a specific IRI including relationships
  like rdfs:subClassOf.
  
  Returns nil if not found."
  [iri]
  (external/vocab iri))

(defn external-iri
  "Returns IRI information for a specific IRI if available in pre-loaded vocabularies.
  
  Returns nil if not found."
  [iri]
  (external/iri iri))


(defn external-context
  "Returns a pre-loaded parsed context for the given URL.
  
  Returns nil if not available."
  [url]
  (external/context url))


(defn compact
  "Compacts an expanded IRI using the provided parsed context or compact function."
  [iri parsed-context-or-fn]
  (compact/compact iri parsed-context-or-fn))


(defn compact-fn
  "Returns a single-arity function that compacts expanded IRIs using the parsed context.
  
  The returned function will attempt to compact an IRI by:
  - First checking for exact matches in the context
  - Then checking for partial matches to create prefixed forms (e.g., \"schema:name\")
  - Returning the original IRI if no match is found
  
  Optional used-atom parameter (a Clojure atom) captures all context terms that were
  actually used during compaction. This is useful when working with large contexts
  (like schema.org) to identify which subset of terms were actually needed."
  ([parsed-context]
   (compact/compact-fn parsed-context nil))
  ([parsed-context used-atom]
   (compact/compact-fn parsed-context used-atom)))


(defn expand-iri
  "Expands a compact IRI to its full form using the parsed context.
  
  vocab? true (default) uses @vocab for properties/classes, false uses @base for @id values."
  ([compact-iri parsed-context]
   (expand/iri compact-iri parsed-context true))
  ([compact-iri parsed-context vocab?]
   (expand/iri compact-iri parsed-context vocab?)))


(defn expand
  "Expands a JSON-LD document to its full form with all context applied.
  
  Takes a JSON-LD node (map) and optional parsed context. If the node contains
  a local @context, it will be merged with any provided parsed context.
  
  Returns an expanded document where:
  - Compact IRIs are expanded to full IRIs
  - JSON-LD keywords (@id, @type, @graph, @list, @value) become Clojure keywords 
    (:id, :type, :graph, :list, :value)
  - Values include metadata:
    - :idx - Path in the original document using get-in syntax (useful for error reporting)
    - :value - The actual value
    - :type - The datatype IRI (if specified)
    - :language - Language tag (if specified)
    - :list - Ordered list values (for @list containers)
  - @graph returns a vector of expanded nodes
  
  Recursively expands into child nodes."
  ([node-map]
   (expand/node node-map {}))
  ([node-map parsed-context]
   (expand/node node-map parsed-context)))


(defn details
  "Expands an IRI and returns [expanded-iri context-settings].
  
  vocab? true (default) uses @vocab for properties/classes, false uses @base for @id values."
  ([compact-iri parsed-context]
   (expand/details compact-iri parsed-context true))
  ([compact-iri parsed-context vocab?]
   (expand/details compact-iri parsed-context vocab?)))


(defn json-ld?
  "Returns true if the document appears to be JSON-LD (has @graph, @context, or @id)."
  [x]
  (boolean
   (or
    (get x "@graph")
    (get-in x [0 "@context"])
    (get-in x [0 "@id"]))))


(defn normalize-data
  "Normalizes JSON-LD data to a consistent string format for comparison/hashing.
  
  Options:
  - :algorithm - :basic (default) or :URDNA2015 (not yet supported)
  - :format - :application/json (default) or :application/n-quads (not yet supported)"
  ([data] (normalize/normalize data))
  ([data opts]
   (normalize/normalize data opts)))
