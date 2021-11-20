(ns fluree.json-ld.compact
  (:require [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn- reverse-context
  "Flips context map from prefix -> prefix-map, to iri -> prefix-map.

  Only includes context items that have an :id; i.e., @id.
  Excludes non-id context statements (e.g., @reverse)."
  [context]
  (loop [[[prefix v] & r] context
         acc {}]
    (if prefix
      (let [iri-fragment (:id v)
            acc*         (cond
                           iri-fragment (assoc acc iri-fragment prefix)
                           (= :vocab prefix) (assoc acc v :vocab)
                           (= :base prefix) (assoc acc v :base)
                           :else acc)]
        (recur r acc*))
      acc)))

(defn compact-fn
  "Returns a single arity function based on the provided context that will compact any string iri."
  [context]
  (let [flipped    (reverse-context context)                ;; flips context map
        match-iris (->> flipped keys (sort-by #(* -1 (count %))))]
    (fn [iri]
      (or
        (some
          (fn [iri-substr]
            (when (str/starts-with? iri iri-substr)         ;; match
              (let [prefix (get flipped iri-substr)
                    suffix (subs iri (count iri-substr))]
                (cond
                  (= :vocab prefix)                         ;; default vocabulary
                  (subs iri (count iri-substr))

                  (= :base prefix)                          ;; base iri
                  (subs iri (count iri-substr))

                  (= "" suffix)                             ;; exact match, no prefix needed, just substitute
                  prefix

                  :else
                  (str prefix ":" suffix)))))
          match-iris)
        iri))))


(defn compact
  "Goes through context and attempts to shorten iri if matches context, else returns original IRI.

  Uses query context format where context values have {:iri 'iri-here'}, so must already be parsed."
  [iri context]
  (let [match-fn (compact-fn context)]
    (match-fn iri)))
