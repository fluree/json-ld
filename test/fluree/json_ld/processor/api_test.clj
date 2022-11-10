(ns fluree.json-ld.processor.api-test
  (:require [fluree.json-ld.processor.api :as jld-processor]
            [clojure.test :as t :refer [deftest is]]
            [clojure.string :as str]))

(def context {"@version" 1.1,
              "address"  "fluree:address",
              "alias"    "fluree:alias",
              "cool"     {"@id" "fluree:cool" "@type" "xsd:boolean"}
              "data"     {"@id" "fluree:data", "@type" "@id"},
              "flakes"   {"@id" "fluree:flakes", "@type" "xsd:long"},
              "fluree"   "https://ns.flur.ee/ledger#",
              "id"       "@id",
              "size"     {"@id" "fluree:size", "@type" "xsd:long"},
              "t"        {"@id" "fluree:t", "@type" "xsd:long"},
              "type"     "@type"})

(def commit
  {"@context" context
   "address"  "",
   "alias"    {"@value" "test/db19" "@language" "en"},
   "data"     {"id"      "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
               "type"    "DB",
               "t"       1,
               "cool"    true
               "address" "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569",
               "flakes"  55,
               "size"    5057}})

(deftest expansion
  (let [result (jld-processor/expand commit)]
    (is (= [{"https://ns.flur.ee/ledger#address" [{"@value" ""}],
             "https://ns.flur.ee/ledger#alias"
             [{"@language" "en", "@value" "test/db19"}],
             "https://ns.flur.ee/ledger#data"
             [{"https://ns.flur.ee/ledger#address"
               [{"@value"
                 "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569"}],
               "https://ns.flur.ee/ledger#cool"
               [{"@type" "xsd:boolean", "@value" true}],
               "https://ns.flur.ee/ledger#flakes"
               [{"@type" "xsd:long", "@value" 55}],
               "@id"
               "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
               "https://ns.flur.ee/ledger#size"
               [{"@type" "xsd:long", "@value" 5057}],
               "https://ns.flur.ee/ledger#t"
               [{"@type" "xsd:long", "@value" 1}],
               "@type" ["DB"]}]}]
           result))))

(deftest compaction
  (let [result (jld-processor/compact (jld-processor/expand commit) context)]
    (is (= commit result))))

(deftest to-rdf
  (let [result (sort (str/split-lines (jld-processor/to-rdf commit)))]
    (is (= ["<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#address> \"fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569\" ."
            "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<xsd:boolean> ."
            "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#flakes> \"55\"^^<xsd:long> ."
            "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#size> \"5057\"^^<xsd:long> ."
            "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<xsd:long> ."
            "_:b0 <https://ns.flur.ee/ledger#address> \"\" ."
            "_:b0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en ."
            "_:b0 <https://ns.flur.ee/ledger#data> <https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> ."]
           result))))

#_(deftest from-rdf
    (let [result (jld-processor/from-rdf (jld-processor/to-rdf commit))]
      (is (= [{"@id" "_:b0",
               "https://ns.flur.ee/ledger#address" [{"@value" ""}],
               "https://ns.flur.ee/ledger#alias"
               [{"@value" "test/db19", "@language" "en"}],
               "https://ns.flur.ee/ledger#data"
               [{"@id"
                 "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6"}]}
              {"@id"
               "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
               "https://ns.flur.ee/ledger#address"
               [{"@value"
                 "fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569"}],
               "https://ns.flur.ee/ledger#cool"
               [{"@value" "true", "@type" "xsd:boolean"}],
               "https://ns.flur.ee/ledger#flakes"
               [{"@value" "55", "@type" "xsd:long"}],
               "https://ns.flur.ee/ledger#size"
               [{"@value" "5057", "@type" "xsd:long"}],
               "https://ns.flur.ee/ledger#t"
               [{"@value" "1", "@type" "xsd:long"}]}]
             result))))

(deftest canonize
  (let [result (jld-processor/canonize commit)]
    (is (= "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#address> \"fluree:memory://e2c8cf4429d7fcd6382fe2c890cf4c3fa2d8d0039b1981ff27e1ee2a05848569\" .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<xsd:boolean> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#flakes> \"55\"^^<xsd:long> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#size> \"5057\"^^<xsd:long> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<xsd:long> .\n_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n_:c14n0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:c14n0 <https://ns.flur.ee/ledger#data> <https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
           result))))
