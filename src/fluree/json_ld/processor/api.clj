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

(defn ->document
  "Takes a document-loader, which takes a url and options and returns a json string
  context document (must have an \"@context\" key with a context as its value)."
  [document-loader url options]
  (let [url-string (.toString ^URI url)]
    (try
      (let [json-string (document-loader url-string options)]
        (JsonDocument/of (io/input-stream (.getBytes ^String json-string))))
      (catch Exception e
        (throw (JsonLdError. JsonLdErrorCode/LOADING_REMOTE_CONTEXT_FAILED
                             (str "Unable to load context: " url-string)))))))

(defrecord PluggableLoader [document-loader]
  DocumentLoader
  (loadDocument [_ url options]
    (->document document-loader url options)))

(defn static-loader
  [url options]
  (if-let [{path :source} (external/context->file url)]
    (slurp (io/resource path))
    (throw (ex-info (str "Unable to load static context: " url)
                    {:url url}))))

(defn expand
  ([json-ld]
   (expand json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> (->json-document json-ld)
       (JsonLd/expand)
       (.loader ^DocumentLoader (->PluggableLoader (:document-loader opts)))
       (.get)
       (parsed))))

(defn compact
  ([json-ld context]
   (compact json-ld context {:document-loader static-loader}))
  ([json-ld context opts]
   (-> (->json-document json-ld)
       (JsonLd/compact (->json-document context))
       (.loader ^DocumentLoader (->PluggableLoader (:document-loader opts)))
       (.get)
       (parsed))))

(defn flatten
  ([json-ld]
   (flatten json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> (->json-document json-ld)
       (JsonLd/flatten)
       (.loader ^DocumentLoader (->PluggableLoader (:document-loader opts)))
       (.get)
       (parsed))))

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
  ([json-ld]
   (to-rdf json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> (->json-document json-ld)
       (JsonLd/toRdf)
       (.loader ^DocumentLoader (->PluggableLoader (:document-loader opts)))
       (.get)
       (.toList)
       (->> (reduce (fn [doc quad] (str doc (->statement quad) "\n")) "")))))

(defn canonize
  ([json-ld]
   (canonize json-ld {:document-loader static-loader}))
  ([json-ld opts]
   (-> (->json-document json-ld)
       (JsonLd/toRdf)
       (.loader ^DocumentLoader (->PluggableLoader (:document-loader opts)))
       (.get)
       (RdfNormalize/normalize)
       (.toList)
       (->> (reduce (fn [doc quad] (str doc (->statement quad) "\n")) "")))))

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
