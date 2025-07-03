(ns fluree.json-ld.processor.api-test
  (:require [fluree.json-ld.processor.api :as jld-processor]
            [clojure.test :as t :refer [deftest is testing]]
            [clojure.string :as str]
            [jsonista.core :as json]))

(def context {"@version" 1.1,
              "address"  "f:address",
              "alias"    "f:alias",
              "cool"     {"@id" "f:cool" "@type" "xsd:boolean"}
              "data"     {"@id" "f:data", "@type" "@id"},
              "DB"       "f:DB",
              "f"        "https://ns.flur.ee/ledger#",
              "id"       "@id",
              "t"        {"@id" "f:t", "@type" "xsd:long"},
              "type"     "@type"
              "xsd"      "http://www.w3.org/2001/XMLSchema#",})

(def commit
  {"@context" context
   "address"  "",
   "alias"    {"@value" "test/db19" "@language" "en"},
   "data"     {"id"      "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
               "type"    "DB",
               "t"       1,
               "cool"    true}})

(def expanded
  [{"https://ns.flur.ee/ledger#address" [{"@value" ""}],
    "https://ns.flur.ee/ledger#alias" [{"@value" "test/db19", "@language" "en"}],
    "https://ns.flur.ee/ledger#data"
    [{"@id" "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "@type" ["https://ns.flur.ee/ledger#DB"],
      "https://ns.flur.ee/ledger#t" [{"@value" 1, "@type" "http://www.w3.org/2001/XMLSchema#long"}],
      "https://ns.flur.ee/ledger#cool" [{"@value" true, "@type" "http://www.w3.org/2001/XMLSchema#boolean"}]}]}])



(deftest expansion
  (testing "inline context"
    (let [result (jld-processor/expand commit)]
      (is (= expanded
             result))))

  (testing "static context"
    (is (= expanded
           (jld-processor/expand (assoc commit "@context"
                                        ["https://ns.flur.ee/ledger#"
                                         {"cool" {"@id" "f:cool" "@type" "xsd:boolean"}}]))))

    (is (= "Unable to load context: http://failure.foo"
           (try (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
                (catch Exception e
                  (:cause (Throwable->map e)))))))

  (testing "remote context"
    (let [test-docloader (fn [_ _] (json/write-value-as-string {"@context" {"foo" "http://example.com/foo#"}}))]
      (is (= [{"http://example.com/foo#bar" [{"@value" 1}]}]
             (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                   {:document-loader test-docloader})))
      (is (= "Unable to load context: foo:context"
             (try (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                        {:document-loader (fn [_ _] (throw (ex-info "borken loader" {})))})
                  (catch Exception e
                    (:cause (Throwable->map e))))))))

  (testing "remote context failure"
    (is (= "Unable to load context: http://failure.foo"
           (try (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
                (catch Exception e
                  (:cause (Throwable->map e))))))))

(deftest compaction
  (let [result (jld-processor/compact (jld-processor/expand commit) context)]
    (is (= commit result))))

(deftest to-rdf
  (let [result (sort (str/split-lines (jld-processor/to-rdf commit)))]
    (is (= ["<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> ."
            "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> ."
            "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> ."
            "_:b0 <https://ns.flur.ee/ledger#address> \"\" ."
            "_:b0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en ."
            "_:b0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> ."]
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
    (is (= "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n_:c14n0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:c14n0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
           result))))
