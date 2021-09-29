(ns fluree.json-ld.expand-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.expand :refer :all]
            [fluree.json-ld :as json-ld]))


(deftest expanding-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [map-ctx (json-ld/parse-context {"schema"  "https://schema.org/"
                                        "REPLACE" "https://schema.org/Person"})
        str-ctx (json-ld/parse-context "https://schema.org")]

    (is (= "https://schema.org/name" (iri "schema:name" map-ctx)))
    (is (= "https://schema.org/Person" (iri "REPLACE" map-ctx)))
    (is (= "https://schema.org/name" (iri "name" str-ctx)))

    ;; not a match, should return unaltered iri
    (is (= "not:matching" (iri "not:matching" map-ctx)))
    ;; have a default vocab, but looks like an iri or compact iri
    (is (= "not:matching" (iri "not:matching" str-ctx)))
    (is (= "http://example.org/ns#Book" (iri "http://example.org/ns#Book" str-ctx)))))


(deftest expanding-reverse-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [ctx (json-ld/parse-context {"schema" "https://schema.org/"
                                    "parent" {"@reverse" "schema:child"}})]
    (is (= (details "parent" ctx)
           ["https://schema.org/child" {:reverse "https://schema.org/child"}]))))
