(ns fluree.json-ld.iri
  (:require [clojure.tools.logging :as log]
            [fluree.json-ld.util :refer [try-catchall]]
            [clojure.string :as str]))

(defn parse-compact-iri
  "Returns 3-tuple of [original-iri prefix suffix] if string looks like
  a compact IRI (i.e. xsd:string or schema:name), else nil.

  Compact IRI must have only one colon (:) and no have a slash before
  or immediately after the colon which ensures it isn't a URL (i.e. https://schema.org)."
  [x]
  (re-matches #"([^:/]+):([^/:][^:]*)" x))


(defn any-iri?
  "Returns true if string has a colon, indicating it is either a compact
  iri or full iri.

  Only to be used when a quick test is needed, not a thorough test."
  [x]
  (str/includes? x ":"))


(defn add-trailing-slash
  "In certain circumstances we want context partial IRIs to end in either a '/' or '#'.
  If it ends in neither, we assume it was meant to end in '/' and add it to the end.

  i.e. if provided https://schema.org, we'll change it to https://schema.org/"
  [partial-iri]
  (if (#{\/ \#} (last partial-iri))
    partial-iri
    (str partial-iri "/")))


(defn parse-prefix
  [s]
  (try-catchall
    (when-let [[_ prefix suffix] (parse-compact-iri s)]
      [prefix suffix])
    (catch e
            (log/warn (str "Error attempting to parse iri: " s))
            (throw e))))
