(ns fluree.json-ld.context
  (:require [fluree.json-ld.iri :as iri]
            [fluree.json-ld.util :as util]))


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
  [orig-context compact-iri]
  (or (when-let [[prefix suffix] (iri/parse-prefix compact-iri)]
        (when-let [full-prefix (or (get orig-context prefix)
                                   (get-in orig-context [prefix "@id"]))]
          (str full-prefix suffix)))
      compact-iri))


(defn parse-value
  "Parses json-ld context value. If a map, iterates over keys."
  [ctx-map-val ctx-original]
  (cond
    (string? ctx-map-val)
    {:id (parse-compact-iri-val ctx-original ctx-map-val)}

    (map? ctx-map-val)
    (reduce-kv
      (fn [acc k v]
        (cond
          (= "@id" k)
          (assoc acc :id (parse-compact-iri-val ctx-original v))

          (= "@type" k)
          (assoc acc :type (->> v
                                util/sequential
                                (mapv (partial parse-compact-iri-val ctx-original))))

          :else
          (assoc acc (keywordize-at-value k) v)))
      {} ctx-map-val)

    :else
    (throw (ex-info "Invalid context provided. Context map values must be a string or map."
                    {:status 400 :error :json-ld/invalid-context}))))


(defn parse-map
  "Parses json-ld context and returns clojure map.
  If an already parsed base-context is provided, merges it into base-context.

  Each context term is a key, and each value a map with term details within. The maps include:
  :id - @id value - the IRI, or IRI substring for the context item
  :vocab - @vocab value - if using a default vocabulary (effectively a blank term). There
           can only be one vocab value for the returned context."
  [base-context context]
  (reduce-kv
    (fn [acc k v]
      (if (= "@vocab" k)
        (assoc-in acc [:vocab :id] (iri/add-trailing-slash v))
        (assoc acc k (parse-value v context))))
    base-context context))


(defn parse
  "Parses json-ld context and returns clojure map.
  If an already parsed base-context is provided, merges it into base-context.

  Each context term is a key, and each value a map with term details within. The maps include:
  :id - @id value - the IRI, or IRI substring for the context item
  :vocab - @vocab value - if using a default vocabulary (effectively a blank term). There
           can only be one vocab value for the returned context."
  ([context] (parse {} context))
  ([base-context context]
   (cond
     (nil? context)
     base-context

     (string? context)
     (assoc base-context :vocab {:id (iri/add-trailing-slash context)})

     (map? context)
     (parse-map base-context context)

     (sequential? context)
     (reduce parse base-context context)

     :else
     (throw (ex-info (str "Invalid json-ld context provided: " context)
                     {:status  400
                      :error   :json-ld/invalid-context
                      :context context})))))
