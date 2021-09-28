(ns fluree.json-ld.context-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.context :refer :all]))


(deftest string-context-parsing
  (testing "Context parsing of a single string"

    ;; with trailing '/'
    (is (= (parse "https://schema.org/")
           {:vocab {:id "https://schema.org/"}}))

    ;; without trailing '/'
    (is (= (parse "https://schema.org")
           {:vocab {:id "https://schema.org/"}}))))


(deftest map-context-parsing
  (testing "Context map parsing"

    ;; string based context
    (is (= (parse {"owl" "http://www.w3.org/2002/07/owl#",
                   "ex"  "http://example.org/ns#"})
           {"owl" {:id "http://www.w3.org/2002/07/owl#"}
            "ex"  {:id "http://example.org/ns#"}}))


    ;; keywords are retained
    (is (= (parse {:owl "http://www.w3.org/2002/07/owl#",
                   :ex  "http://example.org/ns#"})
           {:owl {:id "http://www.w3.org/2002/07/owl#"}
            :ex  {:id "http://example.org/ns#"}}))))


(deftest dependent-context
  (testing "Some contexts use compact IRIs defined in their own document"
    (is (= (parse {"nc"   "http://release.niem.gov/niem/niem-core/4.0/#",
                   "name" "nc:PersonName"})
           {"nc"   {:id "http://release.niem.gov/niem/niem-core/4.0/#"}
            "name" {:id "http://release.niem.gov/niem/niem-core/4.0/#PersonName"}}))))


(deftest multiple-contexts
  (testing "Context map parsing"

    ;; string based context, only one :vocab allowed, should pick last one
    (is (= (parse ["https://schema.org", "http://example.org/ns#"])
           {:vocab {:id "http://example.org/ns#"}}))

    ;; string and map
    (is (= (parse ["https://schema.org",
                   {"owl" "http://www.w3.org/2002/07/owl#",
                    "ex"  "http://example.org/ns#"}])
           {:vocab {:id "https://schema.org/"}
            "owl"  {:id "http://www.w3.org/2002/07/owl#"}
            "ex"   {:id "http://example.org/ns#"}}))

    ;; multiple maps
    (is (= (parse [{"schema" "https://schema.org/"},
                   {"owl" "http://www.w3.org/2002/07/owl#",
                    "ex"  "http://example.org/ns#"}])
           {"schema" {:id "https://schema.org/"}
            "owl"    {:id "http://www.w3.org/2002/07/owl#"}
            "ex"     {:id "http://example.org/ns#"}}))))


(deftest nested-context-details
  (testing "Context keys with map values."

    ;; custom full iri with type defined
    (is (= (parse {"schema"       "https://schema.org/",
                   "customScalar" {"@id"   "https://schema.org/name"
                                   "@type" "https://schema.org/Text"}})
           {"schema"       {:id "https://schema.org/"}
            "customScalar" {:id   "https://schema.org/name"
                            :type ["https://schema.org/Text"]}}))

    (is (= (parse {"schema"      "https://schema.org/",
                   "customClass" {"@id"   "schema:Book"
                                  "@type" ["schema:CreativeWork" "schema:Thing"]}})
           {"schema"      {:id "https://schema.org/"}
            "customClass" {:id   "https://schema.org/Book"
                           :type ["https://schema.org/CreativeWork" "https://schema.org/Thing"]}}))))