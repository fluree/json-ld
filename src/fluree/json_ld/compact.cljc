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


(defn- add-match-to-used-atom
  "If 'used' atom is provided to record all used context values, this
  adds any matches to used atom.

  'used' atom is to return a small context if desired of only matching values,
  as opposed to, e.g. the entire schema.org context containing thousands of items
  when only one might have been actually used."
  [prefix-matched iri used-atom]
  (let [key (if (keyword? prefix-matched)
              (case prefix-matched
                :vocab "@vocab"
                :base "@base"
                ;;else
                (name prefix-matched))
              prefix-matched)]
    (swap! used-atom assoc key iri)))


(defn- partial-iri-match
  "If the input iri is a match to a prefix value,
  e.g. the input iri is http://example.com/ns#t
  with a context of: {'ex' 'http://example.com/ns#'},
  then will return: 'ex:t'.

  Optionally records the usage of that prefix in the used-atom."
  [iri flipped-ctx match-iris used-atom]
  (some
    (fn [iri-substr]
      (when (str/starts-with? iri iri-substr)               ;; match
        (let [prefix (get flipped-ctx iri-substr)
              suffix (subs iri (count iri-substr))]
          (when used-atom
            (add-match-to-used-atom prefix iri-substr used-atom))
          (if (keyword? prefix)
            (if (#{:vocab :base} prefix)
              (subs iri (count iri-substr))
              (keyword (name prefix) suffix))
            (str prefix ":" suffix)))))
    match-iris))


(defn- exact-iri-match
  "If the input iri is an exact match to a context value,
  e.g. the input iri is 'http://example.com/ns#t'
  with a context of: {'t' 'http://example.com/ns#t'},
  then will return: 't'.

  Optionally records the usage of that prefix in the used-atom."
  [iri flipped-ctx used-atom]
  (when-let [exact (get flipped-ctx iri)]
    (when used-atom
      (add-match-to-used-atom exact iri used-atom))
    exact))


(defn compact-fn
  "Returns a single arity function based on the provided context that will compact any string iri.

  Optionally, a second param, used-atom, can be supplied which captures all context items that were used.
  This is useful when a large context is present which only a small subset will be used, and you want to
  know which subset were used."
  ([context] (compact-fn context nil))
  ([context used-atom]
   (let [flipped-ctx (reverse-context context)              ;; flips context map
         match-iris  (->> flipped-ctx
                          keys
                          (filter #(#{\/ \#} (last %)))     ;; only match against iris ending with '#' or '/'
                          (sort-by #(* -1 (count %))))]
     (fn [iri]
       (or (exact-iri-match iri flipped-ctx used-atom)
           (partial-iri-match iri flipped-ctx match-iris used-atom)
           iri)))))


(defn compact
  "Goes through context and attempts to shorten iri if matches context, else returns original IRI.

  Uses query context format where context values have {:iri 'iri-here'}, so must already be parsed."
  [iri parsed-context-or-fn]
  (let [match-fn (if (fn? parsed-context-or-fn)
                   parsed-context-or-fn
                   (compact-fn parsed-context-or-fn))]
    (match-fn iri)))
