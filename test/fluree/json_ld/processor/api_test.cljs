(ns fluree.json-ld.processor.api-test
  (:require [fluree.json-ld.processor.api :as jld-processor]
            [cljs.test :as t :refer [is deftest testing async] :include-macros true]))

(def context {"@version" 1.1,
              "address"  "fluree:address",
              "alias"    "fluree:alias",
              "cool"     {"@id" "fluree:cool" "@type" "xsd:boolean"}
              "data"     {"@id" "fluree:data", "@type" "@id"},
              "DB"       "fluree:DB",
              "fluree"   "https://ns.flur.ee/ledger#",
              "id"       "@id",
              "t"        {"@id" "fluree:t", "@type" "xsd:long"},
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
    [{"@id" "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
      "@type" ["https://ns.flur.ee/ledger#DB"],
      "https://ns.flur.ee/ledger#t" [{"@value" 1, "@type" "http://www.w3.org/2001/XMLSchema#long"}],
      "https://ns.flur.ee/ledger#cool" [{"@value" true, "@type" "http://www.w3.org/2001/XMLSchema#boolean"}]}]}])

(deftest expansion--inline-context
  (async done
         (-> (jld-processor/expand commit)
             (.then (fn [result]
                      (is (= expanded
                             result))
                      (done))))))

(deftest expansion--remote-context
  (async done
         (-> (jld-processor/expand (assoc commit "@context"
                                          ["https://ns.flur.ee/ledger/v1"
                                           {"cool" {"@id" "fluree:cool" "@type" "xsd:boolean"}}]))
             (.then (fn [result]
                      (is (= expanded
                             result))
                      (done))))))

(deftest expansion--remote-context-failure
  (async done
         (-> (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
             (.catch (fn [error]
                       (is (not (nil? error)))
                       (done))))))

(deftest compaction
  (async done
         (-> (jld-processor/expand commit)
             (.then #(jld-processor/compact % context))
             (.then (fn [result]
                      (is (= commit result))
                      (done))))))

(deftest to-rdf
  (async done
         (-> (jld-processor/to-rdf commit)
             (.then (fn [result]
                      (is (= "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:b0 <https://ns.flur.ee/ledger#address> \"\" .\n_:b0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:b0 <https://ns.flur.ee/ledger#data> <https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
                             result))
                      (done))))))

(deftest from-rdf
  (async done
         (-> (jld-processor/to-rdf commit)
             (.then #(jld-processor/from-rdf %))
             (.then (fn [result]
                      (is (= [{"@id" "_:b0",
                               "https://ns.flur.ee/ledger#address" [{"@value" ""}],
                               "https://ns.flur.ee/ledger#alias"
                               [{"@value" "test/db19", "@language" "en"}],
                               "https://ns.flur.ee/ledger#data"
                               [{"@id"
                                 "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6"}]}
                              {"@id"
                               "https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
                               "@type" ["https://ns.flur.ee/ledger#DB"],
                               "https://ns.flur.ee/ledger#cool"
                               [{"@value" "true",
                                 "@type" "http://www.w3.org/2001/XMLSchema#boolean"}],
                               "https://ns.flur.ee/ledger#t"
                               [{"@value" "1",
                                 "@type" "http://www.w3.org/2001/XMLSchema#long"}]}]
                             result))
                      (done))))))

(deftest canonize
  (async done
         (-> (jld-processor/canonize commit)
             (.then (fn [result]
                      (is (= "<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n_:c14n0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:c14n0 <https://ns.flur.ee/ledger#data> <https://ns.flur.ee/ledger#db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
                             result))
                      (done))))))


(comment
  (t/run-tests)



  )
