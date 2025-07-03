(ns fluree.json-ld.processor.api-test
  (:require [fluree.json-ld.processor.api :as jld-processor]
            #?(:clj  [clojure.test :as t :refer [deftest is testing]]
               :cljs [cljs.test :as t :refer [deftest is testing async] :include-macros true])
            #?(:clj [clojure.string :as str])
            #?(:clj [jsonista.core :as json])))

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

(defn json-stringify [data]
  #?(:clj  (json/write-value-as-string data)
     :cljs (.stringify js/JSON (clj->js data))))

(defn extract-error-message [e]
  #?(:clj  (:cause (Throwable->map e))
     :cljs e))

;; Expansion tests
#?(:clj
   (deftest expansion
     (testing "inline context"
       (let [result (jld-processor/expand commit)]
         (is (= expanded result))))

     (testing "static context"
       (is (= expanded
              (jld-processor/expand (assoc commit "@context"
                                           ["https://ns.flur.ee/ledger#"
                                            {"cool" {"@id" "f:cool" "@type" "xsd:boolean"}}]))))

       (is (= "Unable to load context: http://failure.foo"
              (try (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
                   (catch Exception e
                     (extract-error-message e))))))

     (testing "remote context"
       (let [test-docloader (fn [_ _] (json-stringify {"@context" {"foo" "http://example.com/foo#"}}))]
         (is (= [{"http://example.com/foo#bar" [{"@value" 1}]}]
                (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                      {:document-loader test-docloader})))
         (is (= "Unable to load context: foo:context"
                (try (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                           {:document-loader (fn [_ _] (throw (ex-info "broken loader" {})))})
                     (catch Exception e
                       (extract-error-message e)))))))

     (testing "remote context failure"
       (is (= "Unable to load context: http://failure.foo"
              (try (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
                   (catch Exception e
                     (extract-error-message e)))))))

   :cljs
   (deftest expansion
     (testing "inline context"
       (async done
         (-> (jld-processor/expand commit)
             (.then (fn [result]
                      (is (= expanded result))
                      (done))))))

     (testing "static context"
       (async done
         (-> (jld-processor/expand (assoc commit "@context"
                                          ["https://ns.flur.ee/ledger#"
                                           {"cool" {"@id" "f:cool" "@type" "xsd:boolean"}}]))
             (.then (fn [result]
                      (is (= expanded result))
                      (done))))))

     (testing "static context failure"
       (async done
         (-> (jld-processor/expand (assoc commit "@context" "http://failure.foo"))
             (.catch (fn [error]
                       (is (not (nil? error)))
                       (done))))))

     (testing "remote context"
       (async done
         (-> (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                   {:document-loader (fn [_ _]
                                                       (json-stringify {"@context" {"foo" "http://example.com/foo#"}}))})
             (.then (fn [result]
                      (is (= [{"http://example.com/foo#bar" [{"@value" 1}]}]
                             result))
                      (done))))))

     (testing "remote context failure"
       (async done
         (-> (jld-processor/expand {"@context" "foo:context" "foo:bar" 1}
                                   {:document-loader (fn [_ _] (throw (ex-info "Broken loader" {})))})
             (.catch (fn [error]
                      (is (not (nil? error)))
                      (done))))))))

;; Compaction test
#?(:clj
   (deftest compaction
     (let [result (jld-processor/compact (jld-processor/expand commit) context)]
       (is (= commit result))))

   :cljs
   (deftest compaction
     (async done
       (-> (jld-processor/expand commit)
           (.then #(jld-processor/compact % context))
           (.then (fn [result]
                    (is (= commit result))
                    (done)))))))

;; to-rdf test
#?(:clj
   (deftest to-rdf
     (let [result (sort (str/split-lines (jld-processor/to-rdf commit)))]
       (is (= ["<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> ."
               "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> ."
               "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> ."
               "_:b0 <https://ns.flur.ee/ledger#address> \"\" ."
               "_:b0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en ."
               "_:b0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> ."]
              result))))

   :cljs
   (deftest to-rdf
     (async done
       (-> (jld-processor/to-rdf commit)
           (.then (fn [result]
                    (is (= "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:b0 <https://ns.flur.ee/ledger#address> \"\" .\n_:b0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:b0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
                           result))
                    (done)))))))

;; CLJS-only to-rdf remote context test
#?(:cljs
   (deftest to-rdf--remote-context
     (async done
       (-> (jld-processor/to-rdf {"@context" "https://ns.flur.ee/ledger#"
                                  "address" ""})
           (.then (fn [result]
                    (is (= "_:b0 <https://ns.flur.ee/ledger#address> \"\" .\n"
                           result))
                    (done)))))))

;; from-rdf test
#?(:cljs
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
                               "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6"}]}
                            {"@id"
                             "fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6",
                             "@type" ["https://ns.flur.ee/ledger#DB"],
                             "https://ns.flur.ee/ledger#cool"
                             [{"@value" "true",
                               "@type" "http://www.w3.org/2001/XMLSchema#boolean"}],
                             "https://ns.flur.ee/ledger#t"
                             [{"@value" "1",
                               "@type" "http://www.w3.org/2001/XMLSchema#long"}]}]
                           result))
                    (done)))))))

;; flatten test
#?(:clj
   (deftest flatten-test
     (testing "flatten nested structures"
       (let [nested-doc {"@context" {"knows" "http://schema.org/knows"}
                         "@id" "http://example.org/person/1"
                         "knows" {"@id" "http://example.org/person/2"
                                  "knows" {"@id" "http://example.org/person/3"}}}
             result (jld-processor/flatten nested-doc)]
         ;; The result should be a flattened graph
         (is (vector? result))
         ;; Find the @graph entry which contains the flattened nodes
         (let [graph-entry (first (filter #(contains? % "@graph") result))
               nodes (if graph-entry
                       (get graph-entry "@graph")
                       result)]
           ;; Should have at least 2 nodes in the flattened graph
           ;; (person3 might not appear if it has no properties)
           (is (>= (count nodes) 2))
           ;; All nodes should have @id
           (is (every? #(contains? % "@id") nodes))
           ;; Check that the nodes are properly connected via references
           (let [person1 (first (filter #(= "http://example.org/person/1" (get % "@id")) nodes))
                 person2 (first (filter #(= "http://example.org/person/2" (get % "@id")) nodes))]
             (is person1 "person1 should exist")
             (is person2 "person2 should exist")
             ;; person1 knows person2 (as a reference)
             (is (= [{"@id" "http://example.org/person/2"}] 
                    (get person1 "http://schema.org/knows"))
                 "person1 should know person2")
             ;; person2 knows person3 (as a reference)
             (is (= [{"@id" "http://example.org/person/3"}] 
                    (get person2 "http://schema.org/knows"))
                 "person2 should know person3"))))))

   :cljs
   (deftest flatten-test
     (testing "flatten nested structures"
       (async done
         (let [nested-doc {"@context" {"knows" "http://schema.org/knows"}
                           "@id" "http://example.org/person/1"
                           "knows" {"@id" "http://example.org/person/2"
                                    "knows" {"@id" "http://example.org/person/3"}}}]
           (-> (jld-processor/flatten nested-doc)
               (.then (fn [result]
                        ;; The result should be a flattened graph
                        (is (vector? result))
                        ;; Find the @graph entry which contains the flattened nodes
                        (let [graph-entry (first (filter #(contains? % "@graph") result))
                              nodes (if graph-entry
                                      (get graph-entry "@graph")
                                      result)]
                          ;; Should have at least 2 nodes in the flattened graph
                          ;; (person3 might not appear if it has no properties)
                          (is (>= (count nodes) 2))
                          ;; All nodes should have @id
                          (is (every? #(contains? % "@id") nodes))
                          ;; Check that the nodes are properly connected via references
                          (let [person1 (first (filter #(= "http://example.org/person/1" (get % "@id")) nodes))
                                person2 (first (filter #(= "http://example.org/person/2" (get % "@id")) nodes))]
                            (is person1 "person1 should exist")
                            (is person2 "person2 should exist")
                            ;; person1 knows person2 (as a reference)
                            (is (= [{"@id" "http://example.org/person/2"}] 
                                   (get person1 "http://schema.org/knows"))
                                "person1 should know person2")
                            ;; person2 knows person3 (as a reference)
                            (is (= [{"@id" "http://example.org/person/3"}] 
                                   (get person2 "http://schema.org/knows"))
                                "person2 should know person3")))
                        (done)))))))))

;; canonize test
#?(:clj
   (deftest canonize
     (let [result (jld-processor/canonize commit)]
       (is (= "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n_:c14n0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:c14n0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
              result))))

   :cljs
   (deftest canonize
     (async done
       (-> (jld-processor/canonize commit)
           (.then (fn [result]
                    (is (= "<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ns.flur.ee/ledger#DB> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#cool> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n<fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> <https://ns.flur.ee/ledger#t> \"1\"^^<http://www.w3.org/2001/XMLSchema#long> .\n_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n_:c14n0 <https://ns.flur.ee/ledger#alias> \"test/db19\"@en .\n_:c14n0 <https://ns.flur.ee/ledger#data> <fluree:db:sha256:bb3u2hayr4pdwunsa5ijdp7txqrmmku5zlhj7dpozetdcr5g7r5n6> .\n"
                           result))
                    (done)))))))

;; CLJS-only canonize remote context test
#?(:cljs
   (deftest canonize--remote-context
     (async done
       (-> (jld-processor/canonize {"@context" "https://ns.flur.ee/ledger#"
                                    "address" ""})
           (.then (fn [result]
                    (is (= "_:c14n0 <https://ns.flur.ee/ledger#address> \"\" .\n"
                           result))
                    (done)))))))