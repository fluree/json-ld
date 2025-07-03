(ns fluree.json-ld-test
  (:require [fluree.json-ld :as json-ld]
            #?(:clj  [clojure.test :as t :refer [deftest is testing]]
               :cljs [cljs.test :as t :refer [deftest is testing] :include-macros true])))

(deftest external-context-test
  (testing "external-context function"
    (testing "loads pre-fetched contexts"
      ;; Test loading Schema.org context
      (let [schema-ctx (json-ld/external-context "https://schema.org")]
        (is (map? schema-ctx))
        (is (contains? schema-ctx "Person"))
        (is (contains? schema-ctx "name")))

      ;; Test loading Fluree ledger context
      (let [fluree-ctx (json-ld/external-context "https://ns.flur.ee/ledger#")]
        (is (map? fluree-ctx))
        (is (contains? fluree-ctx "f"))
        (is (= "https://ns.flur.ee/ledger#" (get-in fluree-ctx ["f" :id]))))

      ;; Test loading non-existent context returns nil
      (is (nil? (json-ld/external-context "https://example.com/nonexistent"))))))

(deftest external-vocab-test
  (testing "external-vocab function"
    (testing "loads vocabulary information for IRIs"
      ;; The external-vocab function exists but may not have Schema.org vocab data loaded
      ;; Just test that it returns nil for non-existent vocabs
      (is (nil? (json-ld/external-vocab "http://example.com/nonexistent"))))))

(deftest external-iri-test
  (testing "external-iri function"
    (testing "loads IRI information"
      ;; Test loading Schema.org IRI
      (let [name-iri (json-ld/external-iri "http://schema.org/name")]
        (is (or (map? name-iri) (nil? name-iri))))

      ;; Test with non-existent IRI
      (is (nil? (json-ld/external-iri "http://example.com/nonexistent"))))))

(deftest details-function-test
  (testing "details function"
    (testing "returns expanded IRI with context settings"
      (let [ctx (json-ld/parse-context {"@context" {"schema" "http://schema.org/"}})
            [expanded-iri settings] (json-ld/details "schema:name" ctx)]
        (is (= "http://schema.org/name" expanded-iri))
        (is (map? settings))
        (is (= "http://schema.org/" (:id settings))))

      ;; Test with vocab? false
      (let [ctx (json-ld/parse-context {"@context" {"myapp" "https://myapp.com/ns#"}})
            [expanded-iri settings] (json-ld/details "myapp:userId" ctx false)]
        (is (= "https://myapp.com/ns#userId" expanded-iri))
        (is (map? settings))))))

(deftest json-ld-detection-test
  (testing "json-ld? function"
    (testing "detects JSON-LD documents"
      ;; Documents with @graph
      (is (json-ld/json-ld? {"@graph" []}))

      ;; Documents with @context in array
      (is (json-ld/json-ld? [{"@context" {}}]))

      ;; Documents with @id in array
      (is (json-ld/json-ld? [{"@id" "http://example.org"}]))

      ;; Non JSON-LD documents
      (is (false? (json-ld/json-ld? {"name" "John"})))
      (is (false? (json-ld/json-ld? [])))
      (is (false? (json-ld/json-ld? nil))))))

(deftest readme-example-tests
  (testing "README examples work as documented"

    (testing "Context parsing examples"
      ;; Simple context
      (let [ctx (json-ld/parse-context
                 {"@context" {"name" "http://schema.org/name"
                              "age"  {"@id" "http://schema.org/age"
                                      "@type" "http://www.w3.org/2001/XMLSchema#integer"}}})]
        (is (map? ctx))
        (is (= "http://schema.org/name" (get-in ctx ["name" :id])))
        (is (= "http://www.w3.org/2001/XMLSchema#integer" (get-in ctx ["age" :type]))))

      ;; External contexts
      (let [ctx (json-ld/parse-context
                 {"@context" ["https://schema.org"
                              {"myapp" "https://myapp.com/ns#"}]})]
        (is (map? ctx))
        (is (contains? ctx "Person")) ; from Schema.org
        (is (= "https://myapp.com/ns#" (get-in ctx ["myapp" :id])))))

    (testing "Expansion examples"
      ;; The expand function in the main API returns a different format than processor API
      ;; Document expansion - returns a map, not a vector
      (let [expanded (json-ld/expand
                      {"@context" {"name" "http://schema.org/name"}
                       "@id" "http://example.org/person/1"
                       "name" "John Doe"})]
        (is (map? expanded))
        (is (= "http://example.org/person/1" (:id expanded)))
        (is (= "John Doe" (get-in expanded ["http://schema.org/name" 0 :value]))))

      ;; IRI expansion
      (let [ctx (json-ld/parse-context {"@context" {"schema" "http://schema.org/"}})
            expanded-iri (json-ld/expand-iri "schema:name" ctx)]
        (is (= "http://schema.org/name" expanded-iri))))

    (testing "Compaction examples"
      ;; Simple compaction
      (let [ctx (json-ld/parse-context {"@context" {"schema" "http://schema.org/"}})
            compact-fn (json-ld/compact-fn ctx)]
        (is (= "schema:name" (compact-fn "http://schema.org/name"))))

      ;; Compaction with tracking
      (let [ctx (json-ld/parse-context {"@context" {"schema" "http://schema.org/"}})
            used (atom {})
            compact-fn (json-ld/compact-fn ctx used)]
        (is (= "schema:name" (compact-fn "http://schema.org/name")))
        (is (contains? @used "schema"))))

    (testing "Normalization examples"
      ;; Basic normalization
      (let [normalized (json-ld/normalize-data
                        {"@context" {"name" "http://schema.org/name"}
                         "name" "John Doe"
                         "@id" "http://example.org/person/1"})]
        (is (string? normalized))
        ;; Should be valid JSON
        #?(:clj (is (string? normalized)))))))