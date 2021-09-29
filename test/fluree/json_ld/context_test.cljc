(ns fluree.json-ld.context-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.context :refer :all]))


(deftest default-vocabularies
  (testing "A context with a string should be a default vocabulary"

    ;; with trailing '/'
    (is (= (parse "https://schema.org/")
           {:vocab {:id "https://schema.org/"}}))

    ;; without trailing '/'
    (is (= (parse "https://schema.org")
           {:vocab {:id "https://schema.org/"}})))


  (testing "An explicitly defined default vocabulary with @vocab"
    (is (= (parse {"@vocab" "https://schema.org/"})
           {:vocab {:id "https://schema.org/"}})))

  (testing "References to default vocabulary should be concatenated"
    (is (= (parse {"@vocab"     "https://schema.org/"
                   "reverseRef" {"@reverse" "isBasedOn"}
                   "explicit"   "name"
                   "dontTouch"  "https://example.com/ns#42"
                   "id"         "@id"})
           {:vocab       {:id "https://schema.org/"}
            "reverseRef" {:reverse "https://schema.org/isBasedOn"}
            "explicit"   {:id "https://schema.org/name"}
            "dontTouch"  {:id "https://example.com/ns#42"}
            "id"         {:id "@id"}}))))


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


(deftest reverse-refs
  (testing "Reverse refs using full IRI"
    (is (= (parse {"@vocab"       "https://schema.org/"
                   "title"        "https://schema.org/titleEIDR"
                   "derivedWorks" {"@reverse" "https://schema.org/isBasedOn"}})
           {:vocab         {:id "https://schema.org/"},
            "title"        {:id "https://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "https://schema.org/isBasedOn"}})))

  (testing "Reverse refs with compact-iri"
    (is (= (parse {"schema"       "https://schema.org/"
                   "title"        "schema:titleEIDR"
                   "derivedWorks" {"@reverse" "schema:isBasedOn"}})
           {"schema"       {:id "https://schema.org/"},
            "title"        {:id "https://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "https://schema.org/isBasedOn"}}))))

