# fluree/json-ld

A JSON-LD processing library for Clojure and ClojureScript that provides core JSON-LD operations including expansion, compaction, normalization, and RDF conversion.

## Features

- **JSON-LD Processing**: Expand, compact, flatten*, and normalize JSON-LD documents (*flatten has limited ClojureScript support)
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

The library provides the `fluree.json-ld` namespace with JSON-LD operations including:

- Context parsing and manipulation
- IRI expansion and compaction
- Document expansion
- Data normalization
- External context and vocabulary support

## Basic Usage

**Note:** Most operations in this library require a parsed context. Parsing contexts once and reusing them makes expansion and compaction operations more efficient, especially when processing multiple documents with the same context.

**JSON-LD Keywords:** When expanding documents, special JSON-LD keywords (like `@id`, `@type`, `@graph`, `@list`, `@value`) are converted to Clojure keywords (`:id`, `:type`, `:graph`, `:list`, `:value`) in the resulting data structure.

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
;; => {:id "http://example.org/person/1"
;;     :idx []
;;     "http://schema.org/name" [{:value "John Doe" 
;;                                :type nil 
;;                                :idx ["name"]}]}

;; The :idx metadata tracks the path in the original document,
;; useful for error reporting. For nested structures:
(json-ld/expand
  {"@context" {"knows" "http://schema.org/knows"}
   "@id" "http://example.org/person/1"
   "knows" {"@id" "http://example.org/person/2"
            "knows" {"@id" "http://example.org/person/3"}}})
;; => Nested structure with :idx paths like ["knows"] and ["knows" "knows"]

;; Special JSON-LD terms (@id, @type, @graph, etc.) are converted to keywords
(json-ld/expand
  {"@context" {"nick" {"@id" "http://xmlns.com/foaf/0.1/nick"
                       "@container" "@list"}}
   "@id" "http://example.org/people#joebob"
   "nick" ["joe" "bob" "jaybee"]})
;; => {:id "http://example.org/people#joebob"
;;     :idx []
;;     "http://xmlns.com/foaf/0.1/nick" 
;;     [{:list [{:value "joe" :type nil :idx ["nick" 0]}
;;              {:value "bob" :type nil :idx ["nick" 1]}
;;              {:value "jaybee" :type nil :idx ["nick" 2]}]}]}

;; @graph returns a vector of expanded nodes
(json-ld/expand {"@graph" [{"@id" "http://example.org/1" "name" "John"}]})
;; => [{:id "http://example.org/1" :idx ["@graph" 0] ...}]

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

#### Details Function

Get expanded IRI with context settings:

```clojure
;; Get expansion details including context settings
(json-ld/details "schema:name" parsed-context)
;; => ["http://schema.org/name" {:id "schema:name", ...}]

;; Control vocabulary expansion
(json-ld/details "myapp:userId" parsed-context false)
;; => Expands as @id rather than vocabulary term
```

#### JSON-LD Detection

```clojure
;; Check if a document appears to be JSON-LD
(json-ld/json-ld? {"@context" {...} "@id" "..."}) 
;; => true

(json-ld/json-ld? {"name" "John"}) 
;; => false
```

#### External IRI and Vocabulary Functions

```clojure
;; Load external IRIs with additional vocabulary information
(json-ld/external-iri "http://schema.org/Person")
;; => Returns IRI information

(json-ld/external-vocab "http://schema.org/name") 
;; => Returns vocabulary details including rdfs:subClassOf relationships
```

## ClojureScript Support

The library works identically in ClojureScript:

```clojure
(ns my-app.core
  (:require [fluree.json-ld :as json-ld]))

;; All the same functions work in ClojureScript
(def ctx (json-ld/parse-context {"@context" {...}}))
(def expanded (json-ld/expand my-json-ld-doc ctx))
(def compacted-iri (json-ld/compact "http://schema.org/name" ctx))
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

### Prerequisites

ClojureScript tests require Node.js and npm. The Makefile will automatically install npm dependencies when running tests.

### Testing

Run the project's tests:

```bash
$ make test        # Run all tests  
$ make cljtest     # Run Clojure tests only
$ make cljstest    # Run ClojureScript tests only (auto-installs npm deps)
```

### Code Quality

Lint and format code:

```bash
$ make lint        # Run clj-kondo linter
$ make lint-ci     # Run stricter CI linting  
$ make fmt         # Auto-format code
$ make fmt-check   # Check formatting without changing files
```

### Building

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

### Development Tools

Re-parse all external contexts:

```bash
$ make parse-all-contexts
```


## License

Copyright © 2021-2025 Fluree PBC

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.