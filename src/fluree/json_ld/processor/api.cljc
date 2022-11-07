(ns fluree.json-ld.processor.api
  (:refer-clojure :exclude [flatten])
  (:require [jsonista.core :as json])
  #?(:clj (:import (com.apicatalog.jsonld JsonLd)
                   (com.apicatalog.jsonld.document JsonDocument RdfDocument)
                   (java.io StringReader))))

#?(:clojure (set! *warn-on-reflection* true))

#?(:clj
   (defn- ->json-document
     [edn]
     (-> edn
         (json/write-value-as-string)
         (StringReader.)
         (JsonDocument/of))))

#?(:clj
   (defn- ->rdf-document
     [nquads]
     (-> nquads
         (StringReader.)
         (RdfDocument/of))))

#?(:clj
   (defn- ->edn
     [json-ld-api]
     (-> json-ld-api
         (.get)
         (.toString)
         (json/read-value))))

(defn expand
  [json-ld]
  #?(:clj
     (-> json-ld
         (->json-document)
         (JsonLd/expand)
         (->edn))))

(defn compact
  [json-ld context]
  #?(:clj
     (-> json-ld
         (->json-document)
         (JsonLd/compact (->json-document context))
         (->edn))))

(defn flatten
  [json-ld]
  #?(:clj
     (-> json-ld
         (->json-document json-ld)
         (JsonLd/flatten)
         (->edn))))

(defn from-rdf
  [n-quads]
  #?(:clj
     (-> n-quads
         (->rdf-document)
         (JsonLd/fromRdf)
         (->edn))))

#?(:clj
   (defn- ->statement
     [quad]
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
                  (let [datatype (.getDatatype object)
                        lang (.getLanguage object)
                        value (.getValue object)]
                    (str "\"" value "\""
                         (if (.isPresent lang)
                           (str "@" (.get lang))
                           (when (not= "http://www.w3.org/2001/XMLSchema#string" datatype)
                             (str "^^<" datatype ">")))))
                  :else
                  (.toString object)))
          " .")))

(defn to-rdf
  [json-ld]
  #?(:clj
     (-> json-ld
         (->json-document)
         (JsonLd/toRdf)
         (.get)
         (.toList)
         (->> (map ->statement)
              (reduce (fn [doc statement] (str doc statement "\n")) "")))))

#(:clj
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
                  "operations"  {"@id" "fluree:operations", "@type" "@id"}})

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
        "address"
        "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569",
        "flakes" 55,
        "size"   5057}})


    (from-rdf (to-rdf commit))


    [{"@id"
      "https://ns.flur.ee/ledger#commit:sha256:bbr7bbot7cjhbrtcye6inc2zxecfy7zh3ooba5caw6vfkdkbf5uxp",
      "https://ns.flur.ee/ledger#branch"  [{"@value" "main"}],
      "https://ns.flur.ee/ledger#address" [{"@value" ""}],
      "https://ns.flur.ee/ledger#alias"   [{"@language" "en", "@value" "test/db19"}],
      "https://ns.flur.ee/ledger#v"
      [{"@value" "0", "@type" "http://www.w3.org/2001/XMLSchema#decimal"}],
      "https://ns.flur.ee/ledger#time"    [{"@value" "2022-11-07T15:18:55.171018Z"}],
      "https://ns.flur.ee/ledger#issuer"
      [{"@id" "did:fluree:TfHgFTQQiJMHaK1r1qxVPZ3Ridj9pCozqnh"}],
      "https://ns.flur.ee/ledger#data"
      [{"@id"
        "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6"}],
      "@type"                             ["https://ns.flur.ee/ledger#Commit"]}
     {"https://ns.flur.ee/ledger#size"
      [{"@value" "5057", "@type" "http://www.w3.org/2001/XMLSchema#long"}],
      "@id"
      "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "https://ns.flur.ee/ledger#t"
      [{"@value" "1", "@type" "http://www.w3.org/2001/XMLSchema#long"}],
      "https://ns.flur.ee/ledger#flakes"
      [{"@value" "55", "@type" "http://www.w3.org/2001/XMLSchema#long"}],
      "https://ns.flur.ee/ledger#address"
      [{"@value"
        "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569"}],
      "@type" ["https://ns.flur.ee/ledger#DB"]}]

    (compact (expand commit) context)
    {"@context"
     {"message"     "fluree:message",
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
      "operations"  {"@id" "fluree:operations", "@type" "@id"}},
     "branch"  "main",
     "issuer"  "did:fluree:TfHgFTQQiJMHaK1r1qxVPZ3Ridj9pCozqnh",
     "v"       0,
     "id"      "fluree:commit:sha256:bbr7bbot7cjhbrtcye6inc2zxecfy7zh3ooba5caw6vfkdkbf5uxp",
     "address" "",
     "time"    "2022-11-07T15:18:55.171018Z",
     "type"    "Commit",
     "alias"   {"@language" "en", "@value" "test/db19"},
     "data"
     {"id"     "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "flakes" 55,
      "address"
      "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569",
      "t"      1,
      "type"   "DB",
      "size"   5057}}

    ))
