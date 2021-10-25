(ns fluree.json-ld.expand
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.context :as context]))

#?(:clj (set! *warn-on-reflection* true))

(declare node)

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
      (match-default compact-iri context)
      [compact-iri nil]))


(defn iri
  "Expands a compacted iri string to full iri.

  If the iri is not compacted, returns original iri string."
  [compact-iri context]
  (first (details compact-iri context)))


(defmulti parse-node-val (fn [k v ctx-k context]
                             (cond
                               (map? v) :map
                               (sequential? v) :sequential
                               (string? v) :string
                               (number? v) :number
                               :else (throw (ex-info (str "Values in payload must be strings, numbers, maps or vectors. "
                                                          "Provided for key: " k " value: " v)
                                                     {:status 400
                                                      :error  :json-ld/invalid-context})))))

(defmethod parse-node-val :string
  [k v ctx-k context]
  (cond
    (= :id k) (iri v context)
    (= :type k) [(iri v context)]                         ;; always return @type as vector
    :else (let [type (:type ctx-k)]                       ;; type may be defined in the @context
            {:type  type
             :value (if (= :id type)
                      (iri v context)
                      v)})))

(defmethod parse-node-val :number
  [k v ctx-k context]
  {:type  (:type ctx-k)                                   ;; type may be defined in the @context
   :value v})

(defmethod parse-node-val :map
  [k v ctx-k context]
  (let [v*   (dissoc v "@context")
        ctx* (if-let [sub-ctx (get v "@context")]
               (iri context sub-ctx)
               context)]
    (cond
      (contains? v "@list")
      {:type  :list
       :value (mapv #(node % ctx*) (get v "@list"))}

      (contains? v "@value")
      (let [val  (get v "@value")
            type (or (get v "@type")
                     (:type ctx-k)                          ;; if type is defined only in the @context
                     (if (string? val)
                       :xsd/string
                       :xsd/number))]
        {:type  type
         :value (if (= "@id" (get v "@type"))
                  (iri val ctx*)
                  val)})

      ;; single @id key
      (= '("@id") (keys v*))
      {:type  :id
       :value (iri (get v "@id") ctx*)}

      ;; else a sub-value
      :else
      (let [parsed (node v context)]
        {:type  :id
         :value parsed}))))

(defmethod parse-node-val :sequential
  [k v ctx-k context]
  (case k
    :type (mapv #(if (string? %)
                   (iri % context)
                   (throw (ex-info (str "@type values must be strings or vectors of strings, provided: " v)
                                   {:status 400 :error :json-ld/invalid-context})))
                v)
    :list {:type  :list
           :value (mapv #(node % context) v)}
    ;; else
    (mapv #(cond
             (map? %) (node % context)
             (sequential? %) (throw (ex-info (str "Json-ld sequential values within sequential"
                                                  "values is not allowed. Provided key: " k
                                                  " value: " v)
                                             {:status 400 :error :json-ld/invalid-context}))
             :else {:type (:type ctx-k)
                    :value %})
          v)))

(defn node
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([node-map] (node node-map {}))
  ([node-map parsed-context]
   (let [context (context/parse parsed-context (get node-map "@context"))]
     (if-let [graph (get node-map "@graph")]
       (mapv #(node % context) graph)
       (reduce-kv
         (fn [acc k v]
           (let [[k* ctx-k] (details k context)
                 k** (if (= \@ (first k*))
                       (keyword (subs k* 1))
                       k*)
                 v*  (parse-node-val k** v ctx-k context)]
             (assoc acc k** v*)))
         {}
         (dissoc node-map "@context"))))))


