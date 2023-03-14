(ns fluree.json-ld.impl.expand
  (:require [fluree.json-ld.impl.iri :as iri]
            [fluree.json-ld.impl.context :as context]
            [fluree.json-ld.impl.external :as external]
            [fluree.json-ld.impl.util :refer [try-catchall sequential]]))

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
  [compact-iri context vocab?]
  (when-let [default-match (if vocab?
                             (:vocab context)
                             (:base context))]
    (when-not (or (iri/any-iri? compact-iri)
                  (= \@ (first compact-iri)))
      (let [iri (str default-match compact-iri)]
        [iri {:id iri}]))))


(defn details
  "Attempts to match compact-IRI, and if successful returns a two-tuple of
  the full matched IRI along with a map containing any details provided
  in the original context.

  Used primarily with transactions, as if enough details are provided with the context
  we can auto-generate schemas."
  [compact-iri context vocab?]
  (or (match-exact compact-iri context)
      (match-prefix compact-iri context)
      (match-default compact-iri context vocab?)
      [compact-iri (if (string? compact-iri)
                     (if (= \@ (first compact-iri))
                       {:id compact-iri}
                       nil)                                 ;; nil means no match
                     (throw (ex-info (str "Invalid compact-iri: " compact-iri)
                                     {:status 400 :error :json-ld/invalid-iri})))]))


(defn iri
  "Expands a compacted iri string to full iri.

  If the iri is not compacted, returns original iri string."
  [compact-iri context vocab?]
  (first (details compact-iri context vocab?)))

(defn list-item?
  "Returns true if map is a @list.

  A valid @list must be the only key in the map, or optionally can have an @index key"
  [m]
  (and (or (contains? m "@list")
           (contains? m :list))
       (or (= 1 (count m))
           (and (= 2 (count m))
                (or (contains? m "@index")
                    (contains? m :index))))))

(defn set-item?
  "Returns true if map is a @set.

  A valid @set must be the only key in the map, or optionally can have an @index key"
  [m]
  (and (or (contains? m "@set")
           (contains? m :set))
       (or (= 1 (count m))
           (and (= 2 (count m))
                (or (contains? m "@index")
                    (contains? m :index))))))


(defmulti parse-node-val (fn [v _ _ _ idx]
                           (cond
                             (map? v) :map
                             (sequential? v) :sequential
                             (string? v) :string
                             (keyword? v) :keyword
                             (number? v) :number
                             (boolean? v) :boolean
                             (nil? v) :nil
                             :else (throw (ex-info (str "Values in payload must be strings, numbers, maps or vectors. "
                                                        "Provided value: " v " at index: " idx ".")
                                                   {:status 400
                                                    :error  :json-ld/invalid-context})))))

(defmethod parse-node-val :nil
  [v v-info context _ idx]
  nil)

(defmethod parse-node-val :boolean
  [v v-info _ _ idx]
  {:value v
   :type  (:type v-info)                                    ;; type may be defined in the @context
   :idx   idx})

(defmethod parse-node-val :string
  [v {:keys [id type] :as v-info} context _ idx]
  ;; TODO - for both here and :sequential case, we catch @type values if :type-key exists but used explicit anyhow
  ;; TODO - can keep this, but could miss @type-specific context inclusion. Consider changing expansion to use
  ;; TODO - :type-keys as a set and catch before this step as is designed. i.e. :type-keys #{'type' '@type'}
  (cond
    (= "@id" id) (iri v context false)
    (= "@type" id) (iri v context false)                    ;; @type should have been picked up using :type-key, but in case explicitly defined regardless
    (= :id type) {:id  (iri v context false)
                  :idx idx}
    :else {:value v
           :type  type
           :idx   idx}))

;; keywords should only be used in values for IRIs
(defmethod parse-node-val :keyword
  [v {:keys [id type] :as v-info} context _ idx]
  (cond
    (= "@id" id) (iri v context false)
    (= "@type" id) [(iri v context false)]                  ;; @type should have been picked up using :type-key, but in case explicitly defined regardless
    :else {:id  (iri v context false)
           :idx idx}))

(defmethod parse-node-val :number
  [v v-info _ _ idx]
  {:value v
   :type  (:type v-info)                                    ;; type may be defined in the @context
   :idx   idx})

(defmethod parse-node-val :map
  [v v-info context externals idx]
  (let [ctx* (if-let [sub-ctx (get v "@context")]
               (context/parse context sub-ctx)
               context)]
    (cond
      (list-item? v)
      {:list (-> (or (get v "@list")
                     (:list v))
                 (parse-node-val v-info context externals (conj idx "@list")))}

      (set-item? v)                                  ;; set is the default container type, so just flatten to regular vector
      (-> (or (get v "@set")
              (:set v))
          (parse-node-val v-info context externals (conj idx "@set")))

      (or (contains? v "@value")
          (contains? v :value))
      (let [val  (or (get v "@value")
                     (:value v))
            type (if-let [explicit-type (or (get v "@type") (:type v))]
                   (iri explicit-type ctx* true)
                   (:type v-info))]                         ;; if type is defined only in the @context
        (if (#{"@id" :id} (get v "@type"))
          {:id  (iri val ctx* false)
           :idx idx}
          {:value val
           :type  type
           :idx   idx}))

      ;; else a sub-value. Top-level @context might have sub-contexts, if so merge
      :else
      (node v (merge ctx* (:context v-info)) externals idx))))

(defmethod parse-node-val :sequential
  [v v-info context externals idx]
  (let [v* (->> v
                (map-indexed #(cond
                                (map? %2) (if (or (contains? %2 "@value")
                                                  (contains? %2 :value))
                                            (parse-node-val %2 v-info context externals (conj idx %1))
                                            (node %2 context externals (conj idx %1)))

                                (sequential? %2) (throw (ex-info (str "Json-ld sequential values within sequential"
                                                                      "values is not allowed. Provided value: " v
                                                                      " at index: " (conj idx %1) ".")
                                                                 {:status 400
                                                                  :error  :json-ld/invalid-context
                                                                  :idx    (conj idx %1)}))
                                :else
                                (parse-node-val %2 v-info context externals (conj idx %1))))
                (into []))]
    (if (= :list (:container v-info))
      {:list v*}
      v*)))


(defn- type-sub-context
  "The @context can define sub-contexts for certain @type values. Check if exists and merge."
  [context types]
  (reduce
    (fn [context* type]
      (if-let [type-context (get-in context [type :context])]
        (merge context* type-context)
        context*))
    context
    types))


(defn parse-type
  "Parses @type values, returns two-tuple of expanded @type IRIs
  and a (possibly) updated context if there was a type-dependent sub-context present.
  Always return @type as a vector regardless of input."
  [node-map context idx]
  (let [base (if (empty? idx) {:idx []} {:idx idx})
        {:keys [type-key]} context]
    (if-let [type-val (or (get node-map type-key)
                          (get node-map (get-in context [type-key :id])))]
      (let [type-val*     (sequential type-val)
            ;; context may have type-dependent sub-context, update context if so
            context+types (type-sub-context context type-val*)
            expanded      (mapv #(try-catchall
                                   (iri % context true)
                                   (catch e
                                          (throw (ex-info (str "Error parsing @type value, provided: "
                                                               type-val " at index: " (conj (:idx base) (:type-key context)) ".")
                                                          {:status 400
                                                           :error  :json-ld/invalid-context
                                                           :idx    (conj (:idx base) (:type-key context))}))))
                                type-val*)]
        [(assoc base :type expanded) context+types])
      [base context])))

(defn wrap-error
  "Wraps an error happening upstream with :idx value if not present.
  Must be an ex-info formatted error."
  [error idx]
  (if-let [ex-data (ex-data error)]
    (if (:idx ex-data)
      (throw error)
      (throw (ex-info (str (ex-message error) " Error at idx: " idx)
                      (assoc ex-data :idx idx)
                      error)))
    (throw (ex-info (str "Unexpected error: " (ex-message error) " at idx: " idx ".")
                    {:status 500 :error :json-ld/unexpected-error :idx idx}
                    error))))


(defn- node*
  "Does parsing of a node map once normalization happens during 'node' fn.
  node-map should already have @context and @type keys removed as the :type
  will already be included in the base-result."
  [node-map base-result externals context]
  (loop [[[k v] & r] node-map
         context context
         acc     (transient base-result)]
    (if k
      (let [idx* (conj (:idx base-result) k)
            [k* v-info] (try-catchall
                          (details k context true)
                          (catch e (wrap-error e idx*)))
            k**  (if (= \@ (first k*))
                   (keyword (subs k* 1))
                   k*)
            v*   (try-catchall
                   (parse-node-val v v-info context externals idx*)
                   (catch e (wrap-error e idx*)))]
        (recur r
               (if (= :type k**)
                 (type-sub-context context v)
                 context)
               (assoc! acc k** v*)))
      (persistent! acc))))


(defn- expand-nodes
  "Expands a sequence of graph nodes (json objects), ensures any nil nodes removed."
  [context externals idx nodes]
  (loop [[json-node & r] nodes
         i   0
         acc []]
    (if json-node
      (recur r (inc i)
             (conj acc (node json-node context externals (conj idx i))))
      (if (empty? r)
        acc
        (recur r (inc i) acc)))))


(defn node
  "Expands an entire JSON-LD node (JSON object), with optional parsed context
  provided. If node has a local context, will merge with provided parse-context.

  Expands into child nodes."
  ([node-map] (node node-map {} external/external-contexts []))
  ([node-map parsed-context] (node node-map parsed-context external/external-contexts []))
  ([node-map parsed-context externals idx]
   (try-catchall
     (if (sequential? node-map)
       (expand-nodes parsed-context externals idx node-map)
       (let [context   (context/parse parsed-context (or (get node-map "@context")
                                                         (:context node-map)))
             graph-key (cond
                         (contains? node-map "@graph") "@graph"
                         (contains? node-map :graph) :graph)]
         (if-let [graph (get node-map graph-key)]
           (expand-nodes context externals (conj idx "@graph") graph)
           (let [[base-result context*] (parse-type node-map context idx)
                 {:keys [type-key]} context
                 node-map* (dissoc node-map "@context" :context type-key (get-in context [type-key :id]))]
             (node* node-map* base-result externals context*)))))
     (catch e
            (if (ex-data e)
              (throw e)
              (throw (ex-info (str "Invalid JSON-LD. Error occurred at JSON object index: " idx
                                   " with error: " (ex-message e))
                              {:status 400
                               :error  :json-ld/invalid}
                              e)))))))


(comment

  (node {"@context"                  "https://schema.org",
         "@id"                       "https://www.wikidata.org/wiki/Q836821",
         "@type"                     "Movie",
         "name"                      "HELLO The Hitchhiker's Guide to the Galaxy",
         "disambiguatingDescription" "2005 British-American comic science fiction film directed by Garth Jennings",
         "titleEIDR"                 "10.5240/B752-5B47-DBBE-E5D4-5A3F-N",
         "isBasedOn"                 {"@id"    "https://www.wikidata.org/wiki/Q3107329",
                                      "@type"  "Book",
                                      "name"   "The Hitchhiker's Guide to the Galaxy",
                                      "isbn"   "0-330-25864-8",
                                      "author" {"@id"   "https://www.wikidata.org/wiki/Q42"
                                                "@type" "Person"
                                                "name"  "Douglas Adams"}}})

  )
