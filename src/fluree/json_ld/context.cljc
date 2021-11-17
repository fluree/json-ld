(ns fluree.json-ld.context
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.util :as util]
            [fluree.json-ld.external :as external]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(declare parse)

(defn keywordize-at-value
  "If a context key value starts with '@' (i.e. @type, @id), returns
  keywordized version of those keys (i.e. :type, :id)."
  [at-value]
  (if (= \@ (first at-value))
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

(defn- assert-string
  "Throws if provided value is not a string, original key provided for nicer error message"
  [k v]
  (if (string? v)
    v
    (throw (ex-info (str "Invalid @context value in json-ld. " k
                         "must be a string value, provided: " v ".")
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
      {:id (parse-compact-iri-val ctx-original ctx-base default-vocab ctx-val*)}

      (map? ctx-val*)
      (let [map-val (reduce-kv
                      (fn [acc k v]
                        (let [k* (keywordize-at-value k)]
                          (assoc acc k* (cond
                                          (= :type k*)
                                          (->> v
                                               (assert-string k)
                                               (parse-compact-iri-val ctx-original ctx-base default-vocab)
                                               keywordize-at-value)

                                          (#{:id :reverse} k*)
                                          (->> v
                                               (assert-string k)
                                               (parse-compact-iri-val ctx-original ctx-base default-vocab))

                                          (= :context k*)
                                          (parse {} externals v)

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
  [base-context context externals]
  (reduce-kv
    (fn [acc k v]
      (if (and (string? k) (= \@ (first k)))
        (let [kw (keyword (subs k 1))]
          (cond
            (= :vocab kw)
            (assoc-in acc [:vocab :id] (iri/add-trailing-slash v))

            :else                                           ;; something like @protected: true or @version 1.1
            (assoc acc kw v)))
        (assoc acc k (parse-value k v context base-context externals))))
    base-context context))


(defn parse
  "Parses json-ld context and returns clojure map.
  If an already parsed base-context is provided, merges it into base-context.

  Each context term is a key, and each value a map with term details within. The maps include:
  :id - @id value - the IRI, or IRI substring for the context item
  :vocab - @vocab value - if using a default vocabulary (effectively a blank term). There
           can only be one vocab value for the returned context."
  ([context] (parse {} external/external-contexts context))
  ([base-context context] (parse base-context external/external-contexts context))
  ([base-context externals context]
   (cond
     (nil? context)
     base-context

     ;; assume either an external context, or a default vocab
     (string? context)
     (if (externals context)
       (merge base-context (external/context context))
       (assoc base-context :vocab {:id (iri/add-trailing-slash context)}))

     (map? context)
     (parse-map base-context context externals)

     (sequential? context)
     (reduce #(parse %1 externals %2) base-context context)

     :else
     (throw (ex-info (str "Invalid json-ld context provided: " context)
                     {:status  400
                      :error   :json-ld/invalid-context
                      :context context})))))
