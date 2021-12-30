(ns fluree.json-ld.compact-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.compact :refer :all]
            [fluree.json-ld :as jsonld]))

(deftest compacting-iri
  (testing "Compacting a string IRI with context in various forms"
    (let [map-ctx (jsonld/parse-context {"schema"  "http://schema.org/"
                                         "REPLACE" "http://schema.org/Person"
                                         "x"       "schema:x"})
          str-ctx (jsonld/parse-context "https://schema.org")]
      (is (= "x" (compact "http://schema.org/x" map-ctx)))
      (is (= "schema:xyz" (compact "http://schema.org/xyz" map-ctx)))
      (is (= "REPLACE" (compact "http://schema.org/Person" map-ctx)))
      (is (= "name" (compact "http://schema.org/name" str-ctx)))

      ;; not a match, should return unaltered iri
      (is (= "schemas" (compact "schemas" map-ctx)))
      (is (= "http://example.org/ns#blah" (compact "http://example.org/ns#blah" str-ctx)))
      (is (= "http://example.org/ns#blah" (compact "http://example.org/ns#blah" map-ctx))))))


(deftest compacting-function
  (testing "Generating a compacting function produces expected results"
    (let [map-ctx    (jsonld/parse-context {"schema"  "https://schema.org/"
                                            "REPLACE" "https://schema.org/Person"})
          compact-fn (compact-fn map-ctx)]

      (is (= "schema:name" (compact-fn "https://schema.org/name")))
      (is (= "REPLACE" (compact-fn "https://schema.org/Person")))
      (is (= "http://example.org/ns#blah" (compact-fn "http://example.org/ns#blah")))))

  (testing "Generating a compacting function with an atom returns used context items"
    (let [map-ctx    (jsonld/parse-context {"schema"  "http://schema.org/"
                                            "REPLACE" "http://schema.org/Person"})
          used-atom  (atom {})
          compact-fn (compact-fn map-ctx used-atom)]

      (is (= "schema:name" (compact-fn "http://schema.org/name")))
      (is (= {"schema" "http://schema.org/"}
             @used-atom))
      (is (= "REPLACE" (compact-fn "http://schema.org/Person")))
      (is (= {"schema"  "http://schema.org/"
              "REPLACE" "http://schema.org/Person"}
             @used-atom))
      (is (= "http://example.org/ns#blah" (compact-fn "http://example.org/ns#blah")))
      (is (= {"schema"  "http://schema.org/"
              "REPLACE" "http://schema.org/Person"}
             @used-atom)))))


(deftest compact-fn-for-compact
  (testing "A compacting function can optionally be supplied as second param to compact"
    (let [map-ctx    (jsonld/parse-context {"schema"  "https://schema.org/"
                                            "REPLACE" "https://schema.org/Person"})
          compact-fn (compact-fn map-ctx)]
      (is (= "schema:name" (compact "https://schema.org/name" compact-fn)))
      (is (= "REPLACE" (compact "https://schema.org/Person" compact-fn)))
      (is (= "http://example.org/ns#blah" (compact "http://example.org/ns#blah" compact-fn))))))
