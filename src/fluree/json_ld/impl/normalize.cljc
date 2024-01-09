(ns fluree.json-ld.impl.normalize
  (:require [clojure.string :as str]))

;; normalizing json-ld for hashing, cryptographic signatures, etc.
;; test against https://github.com/digitalbazaar/jsonld.js as baseline

;; basic normalization per JSON Canonicalization Scheme https://datatracker.ietf.org/doc/html/rfc8785

;; NOTE - this work is to be done!

(def exp-str->exponent
  {"1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8})

(defn- remove-trailing-decimal
  "For numbers that have decimals, if the decimal is '.0' then remove it"
  [number-as-str]
  (if (str/ends-with? number-as-str ".0")
    (subs number-as-str 0 (- (count number-as-str) 2))
    number-as-str))


(defn- move-decimal
  "Converts an exponent string representation into a standard number
  Used for smaller exponents that the standardization looks to keep as a
  regular number value.

  e.g. 3.333333333333333E8 should instead return 333333333.3333333

  Takes the number portion of the exponent as the first argument, and
  an exponent integer as a second. Using the above example the args would be:
  ['3.333333333333333' 8]"
  [number-as-str exponent]
  (let [[pre-decimal post-decimal] (str/split number-as-str #"\.")]
    (str pre-decimal (subs post-decimal 0 exponent) "." (subs post-decimal exponent))))


(declare basic-normalize-seq)

(defmulti basic-normalize (fn [node]
                            (cond
                              (map? node) :map
                              (sequential? node) :seq
                              (nil? node) :nil
                              (number? node) :number
                              :else :default)))

(defmethod basic-normalize :map
  [node]
  (let [sorted    (sort-by first (into [] node))
        ser-nodes (mapv (fn [[k v]]
                          (str "\"" k "\":" (basic-normalize v)))
                        sorted)]
    (str "{" (str/join "," ser-nodes) "}")))

(defmethod basic-normalize :seq
  [node]
  (let [ser-items (mapv basic-normalize node)]
    (str "[" (str/join "," ser-items) "]")))

(defmethod basic-normalize :nil
  [node]
  "null")

(defmethod basic-normalize :number
  [number]
  (let [n-str (str number)]
    (if (str/includes? n-str "E")
      (let [[num exp] (str/split n-str #"E")]
        (cond
          (str/starts-with? exp "-")
          (str (remove-trailing-decimal num) "e" exp)

          (get exp-str->exponent exp)
          (move-decimal num (get exp-str->exponent exp))

          :else
          (str (remove-trailing-decimal num) "e+" exp)))
      (remove-trailing-decimal n-str))))

(defmethod basic-normalize :default
  [node]
  (pr-str node))



(defn normalize
  ([json-ld] (normalize json-ld {:algorithm :basic
                                 :format    :application/json}))
  ([json-ld {:keys [algorithm format] :or {algorithm :URDNA2015
                                           format    :application/n-quads}}]
   (when (not= algorithm :basic)
     (throw (ex-info (str "Algorithm: " algorithm " not yet implemented!")
                     {:status 400 :error :json-ld/invalid-normalization})))
   (when (not= format :application/json)
     (throw (ex-info (str "Format: " format " not yet implemented!")
                     {:status 400 :error :json-ld/invalid-normalization})))
   (basic-normalize json-ld)))
