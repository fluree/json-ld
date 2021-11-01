(ns fluree.json-ld.expand
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.context :as context]
            [fluree.json-ld.util :refer [try-catchall]]))

;; TODO - differentiate resolution between @type: @id vs @type: @vocab
;; TODO - support @container: @language indexed value
;; TODO - support for @base - applies only to values, not @type or properties (or where explicit @type: @vocab used)

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
      [compact-iri (when (= \@ (first compact-iri))
                     {:id compact-iri})]))


(defn iri
  "Expands a compacted iri string to full iri.

  If the iri is not compacted, returns original iri string."
  [compact-iri context]
  (first (details compact-iri context)))


(defmulti parse-node-val (fn [v _ _ idx]
                           (cond
                             (map? v) :map
                             (sequential? v) :sequential
                             (string? v) :string
                             (number? v) :number
                             :else (throw (ex-info (str "Values in payload must be strings, numbers, maps or vectors. "
                                                        "Provided value: " v " at index: " idx ".")
                                                   {:status 400
                                                    :error  :json-ld/invalid-context})))))

(defmethod parse-node-val :string
  [v {:keys [id type] :as v-info} context idx]
  (cond
    (= "@id" id) (iri v context)
    (= "@type" id) [(iri v context)]                        ;; always return @type as vector
    (= :id type) {:id  (iri v context)
                  :idx idx}
    :else {:value v
           :type  type
           :idx   idx}))

(defmethod parse-node-val :number
  [v v-info _ idx]
  {:value v
   :type  (:type v-info)                                    ;; type may be defined in the @context
   :idx   idx})

(defmethod parse-node-val :map
  [v v-info context idx]
  (let [v*   (dissoc v "@context")
        ctx* (if-let [sub-ctx (get v "@context")]
               (context/parse context sub-ctx)
               context)]
    (cond
      (contains? v "@list")
      {:list (-> (get v "@list")
                 (parse-node-val v-info context (conj idx "@list")))}

      (contains? v "@set")                                  ;; set is the default container type, so just flatten to regular vector
      (-> (get v "@set")
          (parse-node-val v-info context (conj idx "@set")))

      (contains? v "@value")
      (let [val  (get v "@value")
            type (if-let [explicit-type (get v "@type")]
                   (iri explicit-type ctx*)
                   (:type v-info))]                         ;; if type is defined only in the @context
        {:value (if (= "@id" (get v "@type"))
                  (iri val ctx*)
                  val)
         :type  type
         :idx   idx})

      ;; else a sub-value
      :else
      (node v ctx* idx))))

(defmethod parse-node-val :sequential
  [v v-info context idx]
  (if (= "@type" (:id v-info))
    (mapv #(if (string? %)
             (iri % context)
             (throw (ex-info (str "@type values must be strings or vectors of strings, provided: "
                                  v " at index: " idx ".")
                             {:status 400 :error :json-ld/invalid-context})))
          v)
    (let [v* (->> v
                  (map-indexed #(cond
                                  (map? %2) (node %2 context (conj idx %1))
                                  (sequential? %2) (throw (ex-info (str "Json-ld sequential values within sequential"
                                                                        "values is not allowed. Provided value: " v
                                                                        " at index: " (conj idx %1) ".")
                                                                   {:status 400 :error :json-ld/invalid-context}))
                                  :else {:value %2
                                         :type  (:type v-info)
                                         :idx   (conj idx %1)}))
                  (into []))]
      (if (= "@list" (:container v-info))
        {:list v*}
        v*))))

(defn node
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([node-map] (node node-map {}))
  ([node-map parsed-context] (node node-map parsed-context []))
  ([node-map parsed-context idx]
   (try-catchall
     (if (sequential? node-map)
       (map-indexed #(node %2 parsed-context (conj idx %1)) node-map)
       (let [context (context/parse parsed-context (get node-map "@context"))]
         (if-let [graph (get node-map "@graph")]
           (map-indexed #(node %2 context ["@graph" %1]) graph)
           (reduce-kv
             (fn [acc k v]
               (let [[k* v-info] (details k context)
                     k** (if (= \@ (first k*))
                           (keyword (subs k* 1))
                           k*)
                     v*  (parse-node-val v v-info context (conj idx k))]
                 (assoc acc k** v*)))
             (if (empty? idx) {} {:idx idx})
             (dissoc node-map "@context")))))
     (catch e
            (if (ex-data e)
              (throw e)
              (throw (ex-info (str "Invalid JSON-LD. Error occurred at JSON object index: " idx
                                   " with error: " (ex-message e))
                              {:status 400
                               :error  :json-ld/invalid}
                              e)))))))
