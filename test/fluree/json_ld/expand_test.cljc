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



(comment
  (def sample [{"@id" "http://www.w3.org/2002/07/owl#Class"}
               {"@id" "http://www.w3.org/2002/07/owl#DatatypeProperty"}
               {"@id" "http://www.w3.org/2002/07/owl#ObjectProperty"}
               {"@id" "http://www.w3.org/2002/07/owl#Ontology"}

               {"@type"                        ["http://example.org/ns#Book"],
                "http://example.org/ns#author" [{"@id" "_:b1"}]}
               {"@id"                        "_:b1",
                "@type"                      ["http://example.org/ns#Person"],
                "http://example.org/ns#name" [{"@value" "Fred"}]}
               {"@id"   "http://example.org/ns#Book",
                "@type" ["http://www.w3.org/2002/07/owl#Class"]}
               {"@id"   "http://example.org/ns#Person",
                "@type" ["http://www.w3.org/2002/07/owl#Class"]}
               {"@id"   "http://example.org/ns#author",
                "@type" ["http://www.w3.org/2002/07/owl#ObjectProperty"]}
               {"@id"   "http://example.org/ns#name",
                "@type" ["http://www.w3.org/2002/07/owl#DatatypeProperty"]}
               {"@id"   "http://example.org/ns#ontology",
                "@type" ["http://www.w3.org/2002/07/owl#Ontology"]}
               ])

  (def sample2 {"@context" {"owl" "http://www.w3.org/2002/07/owl#",
                            "ex"  "http://example.org/ns#"},
                "@graph"   [{"@id"   "ex:ontology",
                             "@type" "owl:Ontology"}
                            {"@id"   "ex:Book",
                             "@type" "owl:Class"}
                            {"@id"   "ex:Person",
                             "@type" "owl:Class"}
                            {"@id"   "ex:author",
                             "@type" "owl:ObjectProperty"}
                            {"@id"   "ex:name",
                             "@type" "owl:DatatypeProperty"}
                            {"@type"     "ex:Book",
                             "ex:author" {"@id" "_:b1"}}
                            {"@id"     "_:b1",
                             "@type"   "ex:Person",
                             "ex:name" {"@value" "Fred"
                                        "@type"  "xsd:string"}}]})

  (def sample3 {"@context"      {"ical"         "http://www.w3.org/2002/12/cal/ical#",
                                 "xsd"          "http://www.w3.org/2001/XMLSchema#",
                                 "ical:dtstart" {"@type" "xsd:dateTime"}},
                "ical:summary"  "Lady Gaga Concert",
                "ical:location" "New Orleans Arena, New Orleans, Louisiana, USA",
                "ical:dtstart"  "2011-04-09T20:00:00Z"}))