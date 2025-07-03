# fluree/json-ld

A JSON-LD processing library for Clojure and ClojureScript that provides core JSON-LD operations including expansion, compaction, normalization, and RDF conversion.

## Features

- **JSON-LD Processing**: Expand, compact, flatten, and normalize JSON-LD documents
- **Cross-platform**: Works in both Clojure (JVM) and ClojureScript (JS) environments
- **External Context Support**: Pre-loaded contexts for common vocabularies (Schema.org, W3C, Fluree, etc.)
- **RDF Conversion**: Convert between JSON-LD and RDF formats (N-Quads)
- **Context Parsing**: Parse and manipulate JSON-LD contexts

## Installation

Add the following dependency to your `deps.edn`:

```clojure
com.fluree/json-ld {:mvn/version "0.1.0"}
```

## API Overview

The library provides two main namespaces:

- `fluree.json-ld` - Main API for JSON-LD operations
- `fluree.json-ld.processor.api` - Processor API with additional features

## Basic Usage

### Context Parsing

Parse and work with JSON-LD contexts:

```clojure
(require '[fluree.json-ld :as json-ld])

;; Parse a simple context
(def ctx (json-ld/parse-context 
  {"@context" {"name" "http://schema.org/name"
               "age"  {"@id" "http://schema.org/age" 
                       "@type" "http://www.w3.org/2001/XMLSchema#integer"}}}))

;; Parse with external contexts
(def ctx (json-ld/parse-context
  {"@context" ["https://schema.org" 
               {"myapp" "https://myapp.com/ns#"}]}))
```

### Expansion

Expand JSON-LD to its full form:

```clojure
;; Expand a JSON-LD document
(json-ld/expand
  {"@context" {"name" "http://schema.org/name"}
   "@id" "http://example.org/person/1"
   "name" "John Doe"})
;; => [{"@id" "http://example.org/person/1"
;;      "http://schema.org/name" [{"@value" "John Doe"}]}]

;; Expand a single IRI
(json-ld/expand-iri "schema:name" parsed-context)
;; => "http://schema.org/name"
```

### Compaction

Compact expanded JSON-LD using a context:

```clojure
;; Create a compacting function
(def compact-fn (json-ld/compact-fn parsed-context))

(compact-fn "http://schema.org/name")
;; => "schema:name"

;; Track which context terms were used
(def used (atom #{}))
(def compact-fn (json-ld/compact-fn parsed-context used))
(compact-fn "http://schema.org/name")
@used ;; => #{"schema"}
```

### Normalization

Normalize JSON-LD for consistent comparison and hashing:

```clojure
(json-ld/normalize-data
  {"@context" {"name" "http://schema.org/name"}
   "name" "John Doe"
   "@id" "http://example.org/person/1"})
;; => Returns normalized JSON string
```

## Processor API

The processor API provides additional functionality:

```clojure
(require '[fluree.json-ld.processor.api :as jld-processor])

;; Expand with custom document loader
(jld-processor/expand 
  {"@context" "https://schema.org"
   "@type" "Person"
   "name" "Jane Doe"})

;; Compact
(jld-processor/compact
  [{"http://schema.org/name" [{"@value" "Jane Doe"}]}]
  {"@context" {"name" "http://schema.org/name"}})
;; => {"@context" {"name" "http://schema.org/name"}
;;     "name" "Jane Doe"}

;; Flatten nested structures
(jld-processor/flatten
  {"@context" {"knows" "http://schema.org/knows"}
   "@id" "http://example.org/person/1"
   "knows" {"@id" "http://example.org/person/2"
            "knows" {"@id" "http://example.org/person/3"}}})

;; Convert to RDF (N-Quads format)
(jld-processor/to-rdf
  {"@context" {"name" "http://schema.org/name"}
   "@id" "http://example.org/person/1"
   "name" "John Doe"})
;; => "<http://example.org/person/1> <http://schema.org/name> \"John Doe\" .\n"

;; Canonize for consistent output
(jld-processor/canonize
  {"@context" {"name" "http://schema.org/name"}
   "@id" "http://example.org/person/1"
   "name" "John Doe"})
;; => Returns canonicalized N-Quads
```

## ClojureScript Support

The library works identically in ClojureScript:

```clojure
(ns my-app.core
  (:require [fluree.json-ld :as json-ld]
            [fluree.json-ld.processor.api :as jld-processor]))

;; All the same functions work in ClojureScript
(def expanded (json-ld/expand my-json-ld-doc))

;; Async operations in ClojureScript return promises
(.then (jld-processor/expand my-doc)
       (fn [result] (println "Expanded:" result)))
```

## Pre-loaded External Contexts

The library includes pre-parsed contexts for common vocabularies:

- Schema.org: `https://schema.org`
- W3C Credentials: `https://www.w3.org/2018/credentials/v1`
- W3C DID: `https://www.w3.org/ns/did/v1`
- W3C SHACL: `http://www.w3.org/ns/shacl`
- Fluree Ledger: `https://ns.flur.ee/ledger#`
- And more...

Access them directly:

```clojure
;; Load a pre-fetched external context
(json-ld/external-context "https://schema.org")

;; Load vocabulary information for an IRI
(json-ld/external-vocab "http://schema.org/Person")
```

## Development

Run the project's tests:

```bash
$ make test        # Run all tests
$ make cljtest     # Run Clojure tests only
$ make cljstest    # Run ClojureScript tests only
```

Build a deployable jar:

```bash
$ make jar
```

Install locally:

```bash
$ make install
```

Deploy to Clojars (requires `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment variables):

```bash
$ make deploy
```

Re-parse all external contexts:

```bash
$ make parse-all-contexts
```

## License

Copyright © 2021-2025 Fluree PBC

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.