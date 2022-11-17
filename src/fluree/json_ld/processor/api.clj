(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require [jsonista.core :as json]
            [fluree.json-ld.impl.external :as external]
            [clojure.java.io :as io])
  (:import (com.apicatalog.jsonld JsonLd JsonLdError JsonLdErrorCode)
           (com.apicatalog.jsonld.document JsonDocument)
           (com.apicatalog.jsonld.loader DocumentLoader FileLoader DocumentLoaderOptions)
           (com.apicatalog.rdf RdfNQuad)
           (io.setl.rdf.normalization RdfNormalize)
           (java.net URI)
           (java.io StringReader)))

(set! *warn-on-reflection* true)

(defprotocol Parseable
     (parsed [x]))

(extend-protocol Parseable
  ;; JsonArray
  org.glassfish.json.JsonArrayBuilderImpl$JsonArrayImpl
  (parsed [x]
    (into [] (map parsed x)))
  ;; JsonObject
  org.glassfish.json.JsonObjectBuilderImpl$JsonObjectImpl
  (parsed [x]
    (into {} (map (fn [[k v]] [k (parsed v)]) x)))
  org.glassfish.json.JsonNumberImpl
  ;; JsonNumber
  (parsed [x]
    (if (.isIntegral x)
      (.longValue x)
      (.doubleValue x)))
  ;; JsonString
  org.glassfish.json.JsonStringImpl
  (parsed [x]
    (.getString x))

  jakarta.json.JsonValueImpl
  (parsed [x]
    (case (.toString x)
      "true" true
      "false" false
      "null" nil)))

(defn- ->json-document
  ^JsonDocument [edn]
  (-> edn
      (json/write-value-as-string)
      (StringReader.)
      (JsonDocument/of)))

(defrecord StaticLoader []
  DocumentLoader
  (loadDocument [_ url options]
    (if-let [{path :source} (external/context->file (.toString url))]
      (-> (FileLoader.)
          (.loadDocument (.toURI (io/resource path))
                         (DocumentLoaderOptions.)))
      (throw (JsonLdError. JsonLdErrorCode/LOADING_REMOTE_CONTEXT_FAILED
                           (str "Unable to load static context: " (.toString url)))))))

(defn expand
  [json-ld]
  (-> (->json-document json-ld)
      (JsonLd/expand)
      (.loader ^DocumentLoader (->StaticLoader))
      (.get)
      (parsed)))

(defn compact
  [json-ld context]
  (-> (->json-document json-ld)
      (JsonLd/compact (->json-document context))
      (.loader ^DocumentLoader (->StaticLoader))
      (.get)
      (parsed)))

(defn flatten
  [json-ld]
  (-> (->json-document json-ld)
      (JsonLd/flatten)
      (.loader ^DocumentLoader (->StaticLoader))
      (.get)
      (parsed)))

(defn- ->statement
  [^RdfNQuad quad]
  (str (let [subject (.getSubject quad)]
         (if (.isIRI subject)
           (str "<" (.toString subject) ">")
           (.toString subject)))
       " "
       (let [predicate (.getPredicate quad)]
         (if (.isIRI predicate)
           (str "<" (.toString predicate) ">")
           (.toString predicate)))
       " "
       (let [object (.getObject quad)]
         (cond (.isIRI object)
               (str "<" (.toString object) ">")
               (.isLiteral object)
               (let [literal  (.asLiteral object)
                     datatype (.getDatatype literal)
                     lang     (.getLanguage literal)
                     value    (.getValue object)]
                 (str "\"" value "\""
                      (if (.isPresent lang)
                        (str "@" (.get lang))
                        (when (not= "http://www.w3.org/2001/XMLSchema#string" datatype)
                          (str "^^<" datatype ">")))))
               :else                    ; blank node
               (.toString object)))
       " ."))

(defn to-rdf
  [json-ld]
  (-> (->json-document json-ld)
      (JsonLd/toRdf)
      (.loader ^DocumentLoader (->StaticLoader))
      (.get)
      (.toList)
      (->> (reduce (fn [doc quad] (str doc (->statement quad) "\n")) ""))))

(defn canonize
  [json-ld]
  (-> (->json-document json-ld)
      (JsonLd/toRdf)
      (.loader ^DocumentLoader (->StaticLoader))
      (.get)
      (RdfNormalize/normalize)
      (.toList)
      (->> (reduce (fn [doc quad] (str doc (->statement quad) "\n")) ""))))

(comment
  ;; These work up to translating the resulting json-ld back into edn
  #_(defn- ->rdf-document
      [nquads]
      (-> nquads
          (StringReader.)
          (RdfDocument/of)))

  #_(defn from-rdf
      [n-quads]
      ;; TODO: figure out parsing to edn
      (parsed (.get (JsonLd/fromRdf ^RdfDocument (->rdf-document n-quads)))))
  )
