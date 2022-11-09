(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require ["jsonld" :as jldjs]))

(defn compact
  [json-ld context]
  (-> json-ld
      (clj->js)
      (jldjs/compact (clj->js context))
      (.then (fn [result] (js->clj result)))))

(defn expand
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/expand)
      (.then (fn [result] (js->clj result)))))

(defn flatten
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/flatten)
      (.then (fn [result] (js->clj result)))))

(defn from-rdf
  [n-quads]
  (-> n-quads
      (clj->js)
      (jldjs/fromRDF #js{"format" "application/n-quads"})
      (.then (fn [result] (js->clj result)))))

(defn to-rdf
  [json-ld]
  (-> json-ld
      (clj->js)
      (jldjs/toRDF #js{"format" "application/n-quads"})))

(defn canonize
  [json-ld]
  (-> (to-rdf json-ld)
      (.then (fn [rdf]
               (jldjs/canonize rdf #js {"algorithm" "URDNA2015" "inputFormat" "application/n-quads"})))))
