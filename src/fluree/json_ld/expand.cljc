(ns fluree.json-ld.expand
  (:require [fluree.json-ld.iri :as iri]))

#?(:clj (set! *warn-on-reflection* true))

(defn match-exact
  "Attempts to do an exact match with a compact-iri.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context]
  (when-let [exact-match (get context compact-iri)]
    (let [iri (or (:id exact-match)
                  (:reverse exact-match)
                  (throw (ex-info
                           (str "Matching value in context does not contain an @id or @reverse: " compact-iri)
                           {:status 400 :error :json-ld/invalid-iri})))]
      [iri exact-match])))


(defn match-prefix
  "Attempts to do a prefix match with a compact-iri.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context]
  (when-let [[prefix suffix] (iri/parse-prefix compact-iri)]
    (when-let [prefix-match (get context prefix)]
      [(str (:id prefix-match) suffix) prefix-match])))


(defn match-default
  "If context defines a :vocab (default vocab) and compact-iri does
  not look like a full iri (i.e. not https://schema.org/Movie) returns match.
  If successful returns two-tuple of [full-iri context-map-details].
  Else returns nil."
  [compact-iri context]
  (when-let [default-match (:vocab context)]
    (when-not (or (iri/any-iri? compact-iri)
                  (= \@ (first compact-iri)))
      (let [iri (str (:id default-match) compact-iri)]
        [iri {:id iri}]))))


(defn details
  "Attempts to match compact-IRI, and if successful returns a two-tuple of
  the full matched IRI along with a map containing any details provided
  in the original context.

  Used primarily with transactions, as if enough details are provided with the context
  we can auto-generate schemas."
  [compact-iri context]
  (or (match-exact compact-iri context)
      (match-prefix compact-iri context)
      (match-default compact-iri context)))


(defn iri
  "Expands a compacted iri string to full iri.

  If the iri is not compacted, returns original iri string."
  [compact-iri context]
  (if-let [[iri _] (details compact-iri context)]
    iri
    compact-iri))
