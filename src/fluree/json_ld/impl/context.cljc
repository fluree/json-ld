(ns fluree.json-ld.impl.context
  (:require [fluree.json-ld.impl.iri :as iri]
            [fluree.json-ld.impl.util :refer [try-catchall]]
            [fluree.json-ld.impl.external :as external]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(declare parse)

(defn keywordize-at-value
  "If a context key value starts with '@' (i.e. @type, @id), returns
  keywordized version of those keys (i.e. :type, :id)."
  [at-value]
  (if (and (string? at-value) (= \@ (first at-value)))
    (keyword (subs at-value 1))
    at-value))


(defn parse-compact-iri-val
  "A context's value may itself be a compact IRI which refers to
  another key in the original context map.

  If it is a compact IRI, attempts to resolve it, else returns original value

  i.e. with:
  {'nc'   'http://release.niem.gov/niem/niem-core/4.0/#'
   'name' 'nc:PersonName'}
  we ultimately want 'name' to map to http://release.niem.gov/niem/niem-core/4.0/#PersonName"
  [orig-context base-context default-vocab compact-iri]
  (cond
    (str/includes? compact-iri ":")
    (or (when-let [[prefix suffix] (iri/parse-prefix compact-iri)]
          (when-let [full-prefix (or (get orig-context prefix)
                                     (get-in orig-context [prefix "@id"])
                                     (get-in base-context [prefix :id]))]
            (str full-prefix suffix)))
        compact-iri)

    default-vocab
    (if (str/starts-with? compact-iri "@")
      compact-iri
      (str default-vocab compact-iri))

    :else compact-iri))


(defn- recursively-get-id
  "@id values may reference other keys in the original context map, sometimes
  several layers deep. This attempts to get the deepest reference and returns it as the val.

  e.g. in the CLR:
  {'CompactJws':   'dtCompactJws'
   'dtCompactJws': 'dtCompactJws' {'@id':  'clri:dtCompactJws',
                                   '@type': 'xsd:string'}
   ... }

  or:
  {'Address': 'dtAddress',
   'dtAddress': 'clri:dtAddress'
   ... }"
  [compact-iri ctx-original]
  (if-let [compact-iri* (get ctx-original compact-iri)]
    (recur compact-iri* ctx-original)
    compact-iri))

(defn- assert-kw-string
  "Throws if provided value is not a keyword or string, original key provided for nicer error message"
  [k v]
  (if (or (string? v) (keyword? v))
    v
    (throw (ex-info (str "Invalid @context value in json-ld. " k
                         " must be a string value, provided: " v ".")
                    {:status 400
                     :error  :json-ld/invalid-context}))))

(defn- parse-value
  "Parses json-ld context value. If a map, iterates over keys."
  [ctx-key ctx-val ctx-original ctx-base externals]
  (let [default-vocab (when-let [vocab (get ctx-original "@vocab")]
                        (or (get vocab "@id") vocab))
        ctx-val*      (if (string? ctx-val)
                        (recursively-get-id ctx-val ctx-original)
                        ctx-val)]
    (cond
      (string? ctx-val*)
      (let [iri-v (parse-compact-iri-val ctx-original ctx-base default-vocab ctx-val*)]
        (cond-> {:id iri-v}
                (= "@type" iri-v) (assoc :type? true)))

      (map? ctx-val*)
      (let [map-val (reduce-kv
                      (fn [acc k v]
                        (let [k* (keywordize-at-value k)]
                          (assoc acc k* (cond
                                          (= :type k*)
                                          (->> v
                                               (assert-kw-string k)
                                               (parse-compact-iri-val ctx-original ctx-base default-vocab)
                                               keywordize-at-value)

                                          (#{:id :reverse} k*)
                                          (->> v
                                               (assert-kw-string k)
                                               (parse-compact-iri-val ctx-original ctx-base default-vocab))

                                          (= :context k*)
                                          (parse {} externals v)

                                          (= :container k*)
                                          (try-catchall
                                            (if (sequential? v)
                                              (mapv keywordize-at-value v)
                                              (keywordize-at-value v))
                                            (catch e
                                                   (throw (ex-info (str "@container values must be one or more strings that start with @. Provided: " v)
                                                                   {:status 400 :error :json-ld/invalid-context} e))))

                                          :else v))))
                      {} ctx-val*)]
        ;; sometimes a context defines a compact-iri as the key and only includes @type - in this case we need to generate an @id
        (if (or (contains? map-val :id)
                (contains? map-val :reverse))
          map-val
          (assoc map-val :id (parse-compact-iri-val ctx-original ctx-base default-vocab ctx-key))))

      :else
      (throw (ex-info (str "Invalid context provided. Context map values must be a scalars or map. "
                           "Error at value: " ctx-val)
                      {:status 400 :error :json-ld/invalid-context})))))


(defn parse-map
  "Parses json-ld context and returns clojure map.
  If an already parsed base-context is provided, merges it into base-context.

  Each context term is a key, and each value a map with term details within. The maps include:
  :id - @id value - the IRI, or IRI substring for the context item
  :vocab - @vocab value - if using a default vocabulary (effectively a blank term). There
           can only be one vocab value for the returned context."
  [base-context externals context]
  (reduce-kv
    (fn [acc k v]
      (if (and (string? k) (= \@ (first k)))
        (let [kw (keyword (subs k 1))
              v* (cond
                   (= :vocab kw)
                   (if (= "" v)                             ;; an empty string means use @base as @vocab
                     (some-> (or (get context "@base")
                                 (get base-context :base))
                             iri/add-trailing-slash)
                     (iri/add-trailing-slash v))

                   (string? v)
                   (keywordize-at-value v)

                   :else v)]
          (assoc acc kw v*))
        (let [parsed-v (parse-value k v context base-context externals)]
          (cond-> (assoc acc k parsed-v)
            (true? (:type? parsed-v)) (assoc :type-key k)))))
    base-context context))


(defn parse
  "Parses json-ld context and returns clojure map.
  If an already parsed base-context is provided, merges it into base-context.

  Each context term is a key, and each value a map with term details within. The maps include:
  :id - @id value - the IRI, or IRI substring for the context item
  :vocab - @vocab value - if using a default vocabulary (effectively a blank term). There
           can only be one vocab value for the returned context.
  :type-key - The key @type is mapped to (default is @type). If context includes
              e.g. {'type' '@type'} then :type-key would be equal to 'type'.
              It is important to know @type values prior to parsing as context
              may include a sub-context specific to an @type - if those cannot be
              checked easily up front, parsing will be done with the wrong context
              (unless you are lucky and the @type is defined first)."
  ([context] (parse {} external/external-contexts context))
  ([base-context context] (parse base-context external/external-contexts context))
  ([base-context externals context]
   (let [active-context (if (contains? base-context :type-key)
                          base-context
                          ;; :type-key will be replaced while parsing if overridden by context
                          {:type-key "@type"})]
     (cond
       ;; nil resets the context
       (nil? context)
       active-context

       ;; assume either an external context, or a default vocab
       (string? context)
       (if (externals context)
         (merge active-context (external/context context))
         (assoc active-context :vocab (iri/add-trailing-slash context)))

       (map? context)
       (if (contains? context "@context")
         ;; contexts, especially externally loaded, can have a single @context key with context embedded
         (parse active-context (get context "@context" externals))
         (parse-map active-context externals context))

       (sequential? context)
       (reduce (fn [active-context context]
                 (parse active-context externals context))
               active-context
               context)

       :else
       (throw (ex-info (str "Invalid json-ld context provided: " context)
                       {:status 400
                        :error :json-ld/invalid-context
                        :context context}))))))

(comment

  (parse nil
         ["https://ns.flur.ee/ledger/v1"]
         )

  ,)
