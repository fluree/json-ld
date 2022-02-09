(ns fluree.json-ld.compact-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld.compact :as compact]
            [fluree.json-ld :as jsonld]))

(deftest compacting-iri
  (testing "Compacting a string IRI with context in various forms"
    (let [map-ctx (jsonld/parse-context {"schema"  "http://schema.org/"
                                         "REPLACE" "http://schema.org/Person"
                                         "x"       "schema:x"})
          str-ctx (jsonld/parse-context "https://schema.org")]
      (is (= "x" (compact/compact "http://schema.org/x" map-ctx)))
      (is (= "schema:xyz" (compact/compact "http://schema.org/xyz" map-ctx)))
      (is (= "REPLACE" (compact/compact "http://schema.org/Person" map-ctx)))
      (is (= "name" (compact/compact "http://schema.org/name" str-ctx)))

      ;; not a match, should return unaltered iri
      (is (= "schemas" (compact/compact "schemas" map-ctx)))
      (is (= "http://example.org/ns#blah" (compact/compact "http://example.org/ns#blah" str-ctx)))
      (is (= "http://example.org/ns#blah" (compact/compact "http://example.org/ns#blah" map-ctx))))))


(deftest compacting-function
  (testing "Generating a compacting function produces expected results"
    (let [map-ctx    (jsonld/parse-context {"schema"  "https://schema.org/"
                                            "REPLACE" "https://schema.org/Person"})
          compact-fn (compact/compact-fn map-ctx)]

      (is (= "schema:name" (compact-fn "https://schema.org/name")))
      (is (= "REPLACE" (compact-fn "https://schema.org/Person")))
      (is (= "http://example.org/ns#blah" (compact-fn "http://example.org/ns#blah")))))

  (testing "Generating a compacting function with an atom returns used context items"
    (let [map-ctx    (jsonld/parse-context {"schema"  "http://schema.org/"
                                            "REPLACE" "http://schema.org/Person"})
          used-atom  (atom {})
          compact-fn (compact/compact-fn map-ctx used-atom)]

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
          compact-fn (compact/compact-fn map-ctx)]
      (is (= "schema:name" (compact/compact "https://schema.org/name" compact-fn)))
      (is (= "REPLACE" (compact/compact "https://schema.org/Person" compact-fn)))
      (is (= "http://example.org/ns#blah" (compact/compact "http://example.org/ns#blah" compact-fn))))))


(deftest compact-keyword-context
  (testing "When Clojure keywords used in context, properly compact."
    (let [map-ctx    (jsonld/parse-context {:schema  "https://schema.org/"
                                            :replace "https://schema.org/Person"})
          compact-fn (compact/compact-fn map-ctx)]
      (is (= :schema/name (compact/compact "https://schema.org/name" compact-fn)))
      (is (= :replace (compact/compact "https://schema.org/Person" compact-fn)))
      (is (= "http://example.org/ns#blah" (compact/compact "http://example.org/ns#blah" compact-fn))))))
