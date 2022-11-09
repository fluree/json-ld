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

(comment

  (def context {"@version"    1.1,
                "Commit"      "fluree:Commit",
                "CommitProof" "fluree:CommitProof",
                "Context"     "fluree:Context",
                "DB"          "fluree:DB",
                "DID"         "fluree:DID",
                "FNS"         "fluree:FNS",
                "Function"    "fluree:Function",
                "Index"       "fluree:Index",
                "Role"        "fluree:Role",
                "Rule"        "fluree:Rule",
                "address"     "fluree:address",
                "alias"       "fluree:alias",
                "allTypes"    "fluree:allTypes",
                "assert"      {"@id" "fluree:assert", "@container" "@graph"},
                "branch"      "fluree:branch",
                "code"        "fluree:code",
                "commit"      {"@id" "fluree:commit", "@type" "@id"},
                "context"     "fluree:context",
                "cool"        {"@id" "fluree:cool" "@type" "xsd:boolean"}
                "data"        {"@id" "fluree:data", "@type" "@id"},
                "flakes"      {"@id" "fluree:flakes", "@type" "xsd:long"},
                "fluree"      "https://ns.flur.ee/ledger#",
                "functions"   {"@id" "fluree:function", "@type" "@id"},
                "id"          "@id",
                "index"       "fluree:index",
                "issuer"      {"@id" "fluree:issuer", "@type" "@id"},
                "ledger"      "fluree:ledger",
                "ns"          {"@id" "fluree:ns", "@type" "@id"},
                "operations"  {"@id" "fluree:operations", "@type" "@id"}
                "opsAll"      "fluree:opsAll",
                "opsQuery"    "fluree:opsQuery",
                "opsTransact" "fluree:opsTransact",
                "previous"    {"@id" "fluree:previous", "@type" "@id"},
                "rdfs"        "http://www.w3.org/2000/01/rdf-schema#",
                "retract"     {"@id" "fluree:retract", "@container" "@graph"},
                "role"        {"@id" "fluree:role", "@type" "@id"},
                "rules"       {"@id" "fluree:rules", "@type" "@id"},
                "size"        {"@id" "fluree:size", "@type" "xsd:long"},
                "skos"        "http://www.w3.org/2008/05/skos#",
                "t"           {"@id" "fluree:t", "@type" "xsd:long"},
                "tag"         "fluree:tag",
                "time"        "fluree:time",
                "tx"          {"@id" "fluree:tx", "@type" "@id"},
                "type"        "@type",
                "updates"     "fluree:updates",
                "v"           {"@id" "fluree:v", "@type" "xsd:decimal"},
                "xsd"         "http://www.w3.org/2001/XMLSchema#",
                "message"     "fluree:message", })

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
     {"id"      "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "type"    ["DB"],
      "t"       1,
      "cool"    true
      "address" "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569",
      "flakes"  55,
      "size"    5057}}
    )

  (def rdf (atom nil))
  (-> (to-rdf commit)
      (.then #(do (reset! rdf %) %)))

  @rdf

  (def jld (atom nil))
  (-> (from-rdf @rdf)
      (.then #(do (reset! jld %) %)))

  @jld

  (def canonized (atom nil))
  (-> (canonize commit )
      (.then #(do (reset! canonized %) %)))

  @canonized

  (def expanded (atom nil))
  (-> (expand commit)
      (.then #(do (reset! expanded %) %)))

  @expanded

  (def compacted (atom nil))
  (-> (compact @expanded context)
      (.then #(do (reset! compacted %) %)))

  @compacted


  , )
