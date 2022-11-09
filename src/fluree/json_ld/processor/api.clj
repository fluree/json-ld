(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require [jsonista.core :as json])
  (:import (com.apicatalog.jsonld JsonLd)
           (com.apicatalog.jsonld.document JsonDocument RdfDocument)
           (com.apicatalog.rdf RdfNQuad)
           (io.setl.rdf.normalization RdfNormalize)
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
     [edn]
     (-> edn
         (json/write-value-as-string)
         (StringReader.)
         (JsonDocument/of)))

(defn expand
  [json-ld]
  (parsed (.get (JsonLd/expand ^JsonDocument (->json-document json-ld)))))

(defn compact
  [json-ld context]
  (parsed (.get (JsonLd/compact ^JsonDocument (->json-document json-ld)
                                ^JsonDocument (->json-document context)))))

(defn flatten
  [json-ld]
  (parsed (.get (JsonLd/flatten ^JsonDocument (->json-document json-ld)))))

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

(defn- ->rdf-document
    [nquads]
    (-> nquads
        (StringReader.)
        (RdfDocument/of)))

#_(defn from-rdf
  [n-quads]
  (parsed (.get (JsonLd/fromRdf ^RdfDocument (->rdf-document n-quads)))))

(defn to-rdf
  [json-ld]
  (->> (.toList (.get (JsonLd/toRdf ^JsonDocument (->json-document json-ld))))
       (reduce (fn [doc quad] (str doc (->statement quad) "\n")) "")))

(defn canonize
  [json-ld]
  (->> (.toList (RdfNormalize/normalize (.get (JsonLd/toRdf ^JsonDocument (->json-document json-ld)))))
       (reduce (fn [doc quad] (str doc (->statement quad) "\n")) "")))

(comment
  (def context {"message"     "fluree:message",
                "role"        {"@id" "fluree:role", "@type" "@id"},
                "Index"       "fluree:Index",
                "index"       "fluree:index",
                "opsTransact" "fluree:opsTransact",
                "Context"     "fluree:Context",
                "updates"     "fluree:updates",
                "branch"      "fluree:branch",
                "issuer"      {"@id" "fluree:issuer", "@type" "@id"},
                "FNS"         "fluree:FNS",
                "DB"          "fluree:DB",
                "v"           {"@id" "fluree:v", "@type" "xsd:decimal"},
                "id"          "@id",
                "flakes"      {"@id" "fluree:flakes", "@type" "xsd:long"},
                "Role"        "fluree:Role",
                "Commit"      "fluree:Commit",
                "allTypes"    "fluree:allTypes",
                "tag"         "fluree:tag",
                "commit"      {"@id" "fluree:commit", "@type" "@id"},
                "@version"    1.1,
                "rules"       {"@id" "fluree:rules", "@type" "@id"},
                "context"     "fluree:context",
                "address"     "fluree:address",
                "previous"    {"@id" "fluree:previous", "@type" "@id"},
                "fluree"      "https://ns.flur.ee/ledger#",
                "opsQuery"    "fluree:opsQuery",
                "retract"     {"@id" "fluree:retract", "@container" "@graph"},
                "functions"   {"@id" "fluree:function", "@type" "@id"},
                "time"        "fluree:time",
                "t"           {"@id" "fluree:t", "@type" "xsd:long"},
                "rdfs"        "http://www.w3.org/2000/01/rdf-schema#",
                "opsAll"      "fluree:opsAll",
                "Rule"        "fluree:Rule",
                "tx"          {"@id" "fluree:tx", "@type" "@id"},
                "type"        "@type",
                "DID"         "fluree:DID",
                "alias"       "fluree:alias",
                "size"        {"@id" "fluree:size", "@type" "xsd:long"},
                "Function"    "fluree:Function",
                "CommitProof" "fluree:CommitProof",
                "data"        {"@id" "fluree:data", "@type" "@id"},
                "skos"        "http://www.w3.org/2008/05/skos#",
                "code"        "fluree:code",
                "assert"      {"@id" "fluree:assert", "@container" "@graph"},
                "ns"          {"@id" "fluree:ns", "@type" "@id"},
                "ledger"      "fluree:ledger",
                "xsd"         "http://www.w3.org/2001/XMLSchema#",
                "operations"  {"@id" "fluree:operations", "@type" "@id"}
                "cool" {"@id" "fluree:cool" "@type" "xsd:boolean"}})

  (def commit
    {"@context" context
     "branch"   "main",
     "issuer"   {"id" "did:fluree:TfHgFTQQiJMHaK1r1qxVPZ3Ridj9pCozqnh"},
     "v"        0,
     "id"       "fluree:commit:sha256:bbr7bbot7cjhbrtcye6inc2zxecfy7zh3ooba5caw6vfkdkbf5uxp",
     "address"  "",
     "time"     "2022-11-07T15:18:55.171018Z",
     "type"     ["Commit"],
     "alias"    {"@value" "test/db19" "@language" "en"},
     "data"
     {"id"     "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "type"   ["DB"],
      "t"      1,
      "cool"   true
      "address" "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569",
      "flakes" 55,
      "size"   5057}})

  (def rdf (to-rdf commit))

  #_(def jsonld (from-rdf rdf))


  (compact (expand commit) context)
  )
