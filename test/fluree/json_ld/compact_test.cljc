(ns fluree.json-ld.compact-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.compact :refer :all]
            [fluree.json-ld :as jsonld]))

(deftest compacting-iri
  (testing "Compacting a string IRI with context in various forms")
  (let [map-ctx (jsonld/parse-context {"schema"  "https://schema.org/"
                                       "REPLACE" "https://schema.org/Person"})
        str-ctx (jsonld/parse-context "https://schema.org")]

    (is (= "schema:name" (compact "https://schema.org/name" map-ctx)))
    (is (= "REPLACE" (compact "https://schema.org/Person" map-ctx)))
    (is (= "name" (compact "https://schema.org/name" str-ctx)))

    ;; not a match, should return unaltered iri
    (is (= "http://example.org/ns#blah" (compact "http://example.org/ns#blah" str-ctx)))
    (is (= "http://example.org/ns#blah" (compact "http://example.org/ns#blah" map-ctx)))))


(deftest compacting-function
  (testing "Generating a compacting function produces expected results")
  (let [map-ctx (jsonld/parse-context {"schema"  "https://schema.org/"
                                       "REPLACE" "https://schema.org/Person"})
        compact-fn (compact-fn map-ctx)]

    (is (= "schema:name" (compact-fn "https://schema.org/name")))
    (is (= "REPLACE" (compact-fn "https://schema.org/Person")))
    (is (= "http://example.org/ns#blah" (compact-fn "http://example.org/ns#blah")))))