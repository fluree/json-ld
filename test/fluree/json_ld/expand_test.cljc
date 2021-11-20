(ns fluree.json-ld.expand-test
  (:require [clojure.test :refer :all]
            [fluree.json-ld.expand :refer :all]
            [fluree.json-ld :as json-ld]))


(deftest expanding-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [map-ctx (json-ld/parse-context {"schema"  "https://schema.org/"
                                        "REPLACE" "https://schema.org/Person"})
        str-ctx (json-ld/parse-context "https://schema.org")]

    (is (= "https://schema.org/name" (iri "schema:name" map-ctx true)))
    (is (= "https://schema.org/Person" (iri "REPLACE" map-ctx true)))
    (is (= "https://schema.org/name" (iri "name" str-ctx true)))

    ;; not a match, should return unaltered iri
    (is (= "not:matching" (iri "not:matching" map-ctx true)))
    ;; have a default vocab, but looks like an iri or compact iri
    (is (= "not:matching" (iri "not:matching" str-ctx true)))
    (is (= "http://example.org/ns#Book" (iri "http://example.org/ns#Book" str-ctx true)))))


(deftest expanding-reverse-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [ctx (json-ld/parse-context {"schema" "https://schema.org/"
                                    "parent" {"@reverse" "schema:child"}})]
    (is (= (details "parent" ctx true)
           ["https://schema.org/child" {:reverse "https://schema.org/child"}]))))

(deftest expanding-node
  (testing "Datatype in context using compact IRI as key"
    (is (= (node {"@context"      {"ical"         "http://www.w3.org/2002/12/cal/ical#",
                                   "xsd"          "http://www.w3.org/2001/XMLSchema#",
                                   "ical:dtstart" {"@type" "xsd:dateTime"}},
                  "ical:summary"  "Lady Gaga Concert",
                  "ical:location" "New Orleans Arena, New Orleans, Louisiana, USA",
                  "ical:dtstart"  "2011-04-09T20:00:00Z"})
           {"http://www.w3.org/2002/12/cal/ical#summary"
            {:type  nil, :idx ["ical:summary"],
             :value "Lady Gaga Concert"},
            "http://www.w3.org/2002/12/cal/ical#location"
            {:type  nil, :idx ["ical:location"],
             :value "New Orleans Arena, New Orleans, Louisiana, USA"},
            "http://www.w3.org/2002/12/cal/ical#dtstart"
            {:type  "http://www.w3.org/2001/XMLSchema#dateTime", :idx ["ical:dtstart"],
             :value "2011-04-09T20:00:00Z"}})))

  (testing "Nested child with no @id but datatypes"
    (is (= (node {"@context"    {"name"        "http://schema.org/name",
                                 "description" "http://schema.org/description",
                                 "image"       {"@id"   "http://schema.org/image",
                                                "@type" "@id"},
                                 "geo"         "http://schema.org/geo",
                                 "latitude"    {"@id"   "http://schema.org/latitude",
                                                "@type" "xsd:float"},
                                 "longitude"   {"@id"   "http://schema.org/longitude",
                                                "@type" "xsd:float"},
                                 "xsd"         "http://www.w3.org/2001/XMLSchema#"},
                  "name"        "The Empire State Building",
                  "description" "The Empire State Building is a 102-story landmark in New York City.",
                  "image"       "http://www.civil.usherbrooke.ca/cours/gci215a/empire-state-building.jpg",
                  "geo"         {"latitude" "40.75", "longitude" "73.98"}})
           {"http://schema.org/name"
            {:value "The Empire State Building", :type nil, :idx ["name"]},
            "http://schema.org/description"
            {:value "The Empire State Building is a 102-story landmark in New York City.",
             :type  nil, :idx ["description"]},
            "http://schema.org/image"
            {:id  "http://www.civil.usherbrooke.ca/cours/gci215a/empire-state-building.jpg",
             :idx ["image"]},
            "http://schema.org/geo"
            {:idx ["geo"],
             "http://schema.org/latitude"
                  {:value "40.75",
                   :type  "http://www.w3.org/2001/XMLSchema#float",
                   :idx   ["geo" "latitude"]},
             "http://schema.org/longitude"
                  {:value "73.98",
                   :type  "http://www.w3.org/2001/XMLSchema#float",
                   :idx   ["geo" "longitude"]}}})))

  (testing "Nested children with datatypes in context using compact-iris"
    (is (= (node {"gr:includes"               {"@type"     ["gr:Individual" "pto:Vehicle"],
                                               "gr:name"   "Tesla Roadster",
                                               "foaf:page" "http://www.teslamotors.com/roadster"},
                  "@context"                  {"gr"                        "http://purl.org/goodrelations/v1#",
                                               "pto"                       "http://www.productontology.org/id/",
                                               "foaf"                      "http://xmlns.com/foaf/0.1/",
                                               "xsd"                       "http://www.w3.org/2001/XMLSchema#",
                                               "foaf:page"                 {"@type" "@id"},
                                               "gr:acceptedPaymentMethods" {"@type" "@id"},
                                               "gr:hasBusinessFunction"    {"@type" "@id"},
                                               "gr:hasCurrencyValue"       {"@type" "xsd:float"}},
                  "@id"                       "http://example.org/cars/for-sale#tesla",
                  "gr:description"            "Need to sell fast and furiously",
                  "gr:name"                   "Used Tesla Roadster",
                  "gr:hasPriceSpecification"  {"gr:hasCurrencyValue" "85000", "gr:hasCurrency" "USD"},
                  "gr:acceptedPaymentMethods" "gr:Cash",
                  "gr:hasBusinessFunction"    "gr:Sell",
                  "@type"                     "gr:Offering"})

           {:id   "http://example.org/cars/for-sale#tesla",
            :type ["http://purl.org/goodrelations/v1#Offering"],
            "http://purl.org/goodrelations/v1#includes"
                  {:idx  ["gr:includes"],
                   :type ["http://purl.org/goodrelations/v1#Individual"
                          "http://www.productontology.org/id/Vehicle"],
                   "http://purl.org/goodrelations/v1#name"
                         {:value "Tesla Roadster", :type nil,
                          :idx   ["gr:includes" "gr:name"]},
                   "http://xmlns.com/foaf/0.1/page"
                         {:id  "http://www.teslamotors.com/roadster",
                          :idx ["gr:includes" "foaf:page"]}},
            "http://purl.org/goodrelations/v1#description"
                  {:value "Need to sell fast and furiously",
                   :type  nil, :idx ["gr:description"]},
            "http://purl.org/goodrelations/v1#name"
                  {:value "Used Tesla Roadster", :type nil, :idx ["gr:name"]},
            "http://purl.org/goodrelations/v1#hasPriceSpecification"
                  {:idx ["gr:hasPriceSpecification"],
                   "http://purl.org/goodrelations/v1#hasCurrencyValue"
                        {:value "85000", :type "http://www.w3.org/2001/XMLSchema#float",
                         :idx   ["gr:hasPriceSpecification" "gr:hasCurrencyValue"]},
                   "http://purl.org/goodrelations/v1#hasCurrency"
                        {:value "USD", :type nil,
                         :idx   ["gr:hasPriceSpecification" "gr:hasCurrency"]}},
            "http://purl.org/goodrelations/v1#acceptedPaymentMethods"
                  {:id  "http://purl.org/goodrelations/v1#Cash",
                   :idx ["gr:acceptedPaymentMethods"]},
            "http://purl.org/goodrelations/v1#hasBusinessFunction"
                  {:id  "http://purl.org/goodrelations/v1#Sell",
                   :idx ["gr:hasBusinessFunction"]}})))


  (testing "Nested children in vector"
    (is (= (node {"@context"     {"name"         "http://rdf.data-vocabulary.org/#name",
                                  "ingredient"   "http://rdf.data-vocabulary.org/#ingredients",
                                  "yield"        "http://rdf.data-vocabulary.org/#yield",
                                  "instructions" "http://rdf.data-vocabulary.org/#instructions",
                                  "step"         {"@id" "http://rdf.data-vocabulary.org/#step", "@type" "xsd:integer"},
                                  "description"  "http://rdf.data-vocabulary.org/#description",
                                  "xsd"          "http://www.w3.org/2001/XMLSchema#"},
                  "name"         "Mojito",
                  "ingredient"   ["12 fresh mint leaves"
                                  "1/2 lime, juiced with pulp"
                                  "1 tablespoons white sugar"
                                  "1 cup ice cubes"
                                  "2 fluid ounces white rum"
                                  "1/2 cup club soda"],
                  "yield"        "1 cocktail",
                  "instructions" [{"step" 1, "description" "Crush lime juice, mint and sugar together in glass."}
                                  {"step" 2, "description" "Fill glass to top with ice cubes."}
                                  {"step" 3, "description" "Pour white rum over ice."}
                                  {"step" 4, "description" "Fill the rest of glass with club soda, stir."}
                                  {"step" 5, "description" "Garnish with a lime wedge."}]})
           {"http://rdf.data-vocabulary.org/#name"
            {:type nil, :idx ["name"], :value "Mojito"},
            "http://rdf.data-vocabulary.org/#ingredients"
            [{:type nil, :idx ["ingredient" 0], :value "12 fresh mint leaves"}
             {:type nil, :idx ["ingredient" 1], :value "1/2 lime, juiced with pulp"}
             {:type nil, :idx ["ingredient" 2], :value "1 tablespoons white sugar"}
             {:type nil, :idx ["ingredient" 3], :value "1 cup ice cubes"}
             {:type nil, :idx ["ingredient" 4], :value "2 fluid ounces white rum"}
             {:type nil, :idx ["ingredient" 5], :value "1/2 cup club soda"}],
            "http://rdf.data-vocabulary.org/#yield"
            {:type nil, :idx ["yield"], :value "1 cocktail"},
            "http://rdf.data-vocabulary.org/#instructions"
            [{:idx ["instructions" 0],
              "http://rdf.data-vocabulary.org/#step"
                   {:type "http://www.w3.org/2001/XMLSchema#integer",
                    :idx  ["instructions" 0 "step"], :value 1},
              "http://rdf.data-vocabulary.org/#description"
                   {:type  nil, :idx ["instructions" 0 "description"],
                    :value "Crush lime juice, mint and sugar together in glass."}}
             {:idx ["instructions" 1],
              "http://rdf.data-vocabulary.org/#step"
                   {:type "http://www.w3.org/2001/XMLSchema#integer",
                    :idx  ["instructions" 1 "step"], :value 2},
              "http://rdf.data-vocabulary.org/#description"
                   {:type  nil, :idx ["instructions" 1 "description"],
                    :value "Fill glass to top with ice cubes."}}
             {:idx ["instructions" 2],
              "http://rdf.data-vocabulary.org/#step"
                   {:type "http://www.w3.org/2001/XMLSchema#integer",
                    :idx  ["instructions" 2 "step"], :value 3},
              "http://rdf.data-vocabulary.org/#description"
                   {:type  nil, :idx ["instructions" 2 "description"],
                    :value "Pour white rum over ice."}}
             {:idx ["instructions" 3],
              "http://rdf.data-vocabulary.org/#step"
                   {:type "http://www.w3.org/2001/XMLSchema#integer",
                    :idx  ["instructions" 3 "step"], :value 4},
              "http://rdf.data-vocabulary.org/#description"
                   {:type  nil, :idx ["instructions" 3 "description"],
                    :value "Fill the rest of glass with club soda, stir."}}
             {:idx ["instructions" 4],
              "http://rdf.data-vocabulary.org/#step"
                   {:type "http://www.w3.org/2001/XMLSchema#integer",
                    :idx  ["instructions" 4 "step"], :value 5},
              "http://rdf.data-vocabulary.org/#description"
                   {:type  nil, :idx ["instructions" 4 "description"],
                    :value "Garnish with a lime wedge."}}]}))))

(deftest node-graph-parse
  (testing "Parse node that is a graph"
    (is (= (into []
                 (node {"@context" {"dc11"        "http://purl.org/dc/elements/1.1/",
                                    "ex"          "http://example.org/vocab#",
                                    "xsd"         "http://www.w3.org/2001/XMLSchema#",
                                    "ex:contains" {"@type" "@id"}},
                        "@graph"   [{"@id"         "http://example.org/library",
                                     "@type"       "ex:Library",
                                     "ex:contains" "http://example.org/library/the-republic"}
                                    {"@id"          "http://example.org/library/the-republic",
                                     "@type"        "ex:Book",
                                     "dc11:creator" "Plato",
                                     "dc11:title"   "The Republic",
                                     "ex:contains"  "http://example.org/library/the-republic#introduction"}
                                    {"@id"              "http://example.org/library/the-republic#introduction",
                                     "@type"            "ex:Chapter",
                                     "dc11:description" "An introductory chapter on The Republic.",
                                     "dc11:title"       "The Introduction"}]}))
           [{:idx  ["@graph" 0],
             :id   "http://example.org/library",
             :type ["http://example.org/vocab#Library"],
             "http://example.org/vocab#contains"
                   {:id "http://example.org/library/the-republic", :idx ["@graph" 0 "ex:contains"]}}
            {:idx  ["@graph" 1],
             :id   "http://example.org/library/the-republic",
             :type ["http://example.org/vocab#Book"],
             "http://purl.org/dc/elements/1.1/creator"
                   {:value "Plato", :type nil, :idx ["@graph" 1 "dc11:creator"]},
             "http://purl.org/dc/elements/1.1/title"
                   {:value "The Republic", :type nil, :idx ["@graph" 1 "dc11:title"]},
             "http://example.org/vocab#contains"
                   {:id  "http://example.org/library/the-republic#introduction",
                    :idx ["@graph" 1 "ex:contains"]}}
            {:idx  ["@graph" 2],
             :id   "http://example.org/library/the-republic#introduction",
             :type ["http://example.org/vocab#Chapter"],
             "http://purl.org/dc/elements/1.1/description"
                   {:value "An introductory chapter on The Republic.", :type nil,
                    :idx   ["@graph" 2 "dc11:description"]},
             "http://purl.org/dc/elements/1.1/title"
                   {:value "The Introduction", :type nil,
                    :idx   ["@graph" 2 "dc11:title"]}}]))))


(deftest list-type-values
  (testing "An @list can be used in context to specify all instances of the node are list items"
    (is (= (node {"@context" {"nick" {"@id"        "http://xmlns.com/foaf/0.1/nick",
                                      "@container" "@list"}}

                  "@id"      "http://example.org/people#joebob",
                  "nick"     ["joe", "bob", "jaybee"]})
           {:id "http://example.org/people#joebob",
            "http://xmlns.com/foaf/0.1/nick"
                {:list [{:value "joe", :type nil, :idx ["nick" 0]}
                        {:value "bob", :type nil, :idx ["nick" 1]}
                        {:value "jaybee", :type nil, :idx ["nick" 2]}]}})))
  (testing "An @list can also be used within the node itself"
    (is (= (node {"@context"  {"foaf" "http://xmlns.com/foaf/0.1/"}
                  "@id"       "http://example.org/people#joebob",
                  "foaf:nick" {"@list" ["joe", "bob", "jaybee"]}})
           {:id "http://example.org/people#joebob",
            "http://xmlns.com/foaf/0.1/nick"
                {:list [{:value "joe", :type nil, :idx ["foaf:nick" "@list" 0]}
                        {:value "bob", :type nil, :idx ["foaf:nick" "@list" 1]}
                        {:value "jaybee", :type nil, :idx ["foaf:nick" "@list" 2]}]}}))))


(deftest set-type-values
  (testing "An @set is the default list/vector container type, even if specified, flatten."
    (is (= (node {"@context"  {"foaf" "http://xmlns.com/foaf/0.1/"}
                  "@id"       "http://example.org/people#joebob",
                  "foaf:nick" {"@set" ["joe", "bob", "jaybee"]}})
           {:id "http://example.org/people#joebob",
            "http://xmlns.com/foaf/0.1/nick"
                [{:value "joe", :type nil, :idx ["foaf:nick" "@set" 0]}
                 {:value "bob", :type nil, :idx ["foaf:nick" "@set" 1]}
                 {:value "jaybee", :type nil, :idx ["foaf:nick" "@set" 2]}]})))
  (testing "An @set can be used in context ensure all values presented in vector/array format."
    (is (= (node {"@context" {"nick" {"@id"        "http://xmlns.com/foaf/0.1/nick",
                                      "@container" "@set"}}

                  "@id"      "http://example.org/people#joebob",
                  "nick"     ["joe", "bob", "jaybee"]})
           {:id "http://example.org/people#joebob",
            "http://xmlns.com/foaf/0.1/nick"
                [{:value "joe", :type nil, :idx ["nick" 0]}
                 {:value "bob", :type nil, :idx ["nick" 1]}
                 {:value "jaybee", :type nil, :idx ["nick" 2]}]}))))


(deftest base-and-vocab
  (testing "An @base and a @vocab expand the correct iris"
    (is (= (node {"@context"    {"@base"       "https://base.com/base/iri"
                                 "@vocab"      "https://vocab.com/vocab/iri/"
                                 "iriProperty" {"@type" "@id"}}
                  "@id"         "#joebob",
                  "@type"       "Joey"
                  "name"        "Joe Bob"
                  "iriProperty" "#a-relative-id"})
           {:id   "https://base.com/base/iri#joebob",
            :type ["https://vocab.com/vocab/iri/Joey"]
            "https://vocab.com/vocab/iri/name"
                  {:value "Joe Bob" :type nil :idx ["name"]}
            "https://vocab.com/vocab/iri/iriProperty"
                  {:id  "https://base.com/base/iri#a-relative-id"
                   :idx ["iriProperty"]}}))))