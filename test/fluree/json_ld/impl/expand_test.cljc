(ns fluree.json-ld.impl.expand-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld :as json-ld]
            [fluree.json-ld.impl.expand :as expand])
  (:import (clojure.lang ExceptionInfo)))

(deftest expanding-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [map-ctx (json-ld/parse-context {"schema"  "http://schema.org/"
                                        "REPLACE" "http://schema.org/Person"})
        str-ctx (json-ld/parse-context "https://schema.org")]

    (is (= "http://schema.org/name" (expand/iri "schema:name" map-ctx true)))
    (is (= "http://schema.org/Person" (expand/iri "REPLACE" map-ctx true)))
    (is (= "http://schema.org/name" (expand/iri "name" str-ctx true)))

    ;; not a match, should return unaltered iri
    (is (= "not:matching" (expand/iri "not:matching" map-ctx true)))
    ;; have a default vocab, but looks like an iri or compact iri
    (is (= "not:matching" (expand/iri "not:matching" str-ctx true)))
    (is (= "http://example.org/ns#Book" (expand/iri "http://example.org/ns#Book" str-ctx true)))))


(deftest expanding-reverse-iri
  (testing "Expanding a compacted IRI with context in various forms")
  (let [ctx (json-ld/parse-context {"schema" "http://schema.org/"
                                    "parent" {"@reverse" "schema:child"}})]
    (is (= ["http://schema.org/child" {:reverse "http://schema.org/child"}]
           (expand/details "parent" ctx true)))))

(deftest expanding-node
  (testing "Datatype in context using compact IRI as key"
    (is (= {"http://www.w3.org/2002/12/cal/ical#summary"
            {:type  nil, :idx ["ical:summary"],
             :value "Lady Gaga Concert"},
            "http://www.w3.org/2002/12/cal/ical#location"
            {:type  nil, :idx ["ical:location"],
             :value "New Orleans Arena, New Orleans, Louisiana, USA"},
            "http://www.w3.org/2002/12/cal/ical#dtstart"
            {:type  "http://www.w3.org/2001/XMLSchema#dateTime", :idx ["ical:dtstart"],
             :value "2011-04-09T20:00:00Z"}
            :idx []}
           (expand/node {"@context"      {"ical"         "http://www.w3.org/2002/12/cal/ical#",
                                          "xsd"          "http://www.w3.org/2001/XMLSchema#",
                                          "ical:dtstart" {"@type" "xsd:dateTime"}},
                         "ical:summary"  "Lady Gaga Concert",
                         "ical:location" "New Orleans Arena, New Orleans, Louisiana, USA",
                         "ical:dtstart"  "2011-04-09T20:00:00Z"}))))

  (testing "Using a context mapped to use 'type' and 'id', but using @type and @id instead"
    (is (= {:id   "https://www.wikidata.org/wiki/Q836821"
            :type "http://schema.org/Movie"
            "http://schema.org/name"
            {:value "Hitchhiker's Guide to the Galaxy", :type nil, :idx ["name"]}
            :idx  []}
           (expand/node {"@context" "https://schema.org"
                         "@id"      "https://www.wikidata.org/wiki/Q836821"
                         "@type"    "Movie"
                         "name"     "Hitchhiker's Guide to the Galaxy"}))))

  (testing "Sequential values containing maps with @values"
    (is (= {:idx                                            [],
            :type                                           ["http://www.w3.org/2002/07/owl#Class"],
            :id                                             "https://ontologies.semanticarts.com/gist/CoherentUnit",
            "http://www.w3.org/2004/02/skos/core#scopeNote" [{:value "Coherent unit is the physics term for this, informally you might think of it as the standard unit for a given dimension.",
                                                              :type  "http://www.w3.org/2001/XMLSchema#string",
                                                              :idx   ["skos:scopeNote" 0]}
                                                             {:value "In principle, the CoherentUnit for a ProductUnit or RatioUnit can be inferred by recursively decomposing the products and ratios into their respective CoherentUnits, bottoming out in SimpleUnits",
                                                              :type  "http://www.w3.org/2001/XMLSchema#string",
                                                              :idx   ["skos:scopeNote" 1]}]}
           (expand/node {"@context"       {"gist" "https://ontologies.semanticarts.com/gist/",
                                           "owl"  "http://www.w3.org/2002/07/owl#",
                                           "skos" "http://www.w3.org/2004/02/skos/core#",
                                           "xsd"  "http://www.w3.org/2001/XMLSchema#"},
                         "@id"            "gist:CoherentUnit",
                         "skos:scopeNote" [{"@type" "xsd:string", "@value" "Coherent unit is the physics term for this, informally you might think of it as the standard unit for a given dimension."}
                                           {"@type" "xsd:string", "@value" "In principle, the CoherentUnit for a ProductUnit or RatioUnit can be inferred by recursively decomposing the products and ratios into their respective CoherentUnits, bottoming out in SimpleUnits"}],
                         "@type"          "owl:Class"}))))

  (testing "Nested child with no @id but datatypes"
    (is (= {"http://schema.org/name"
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
              :idx   ["geo" "longitude"]}}
            :idx []}
           (expand/node {"@context"    {"name"        "http://schema.org/name",
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
                         "geo"         {"latitude" "40.75", "longitude" "73.98"}}))))

  (testing "Nested children with datatypes in context using compact-iris"
    (is (= {:id   "http://example.org/cars/for-sale#tesla",
            :idx  [],
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
             :idx ["gr:hasBusinessFunction"]}
            "my:json"
            {:value {"foo" {:bar [1 false 9.0 nil]}},
             :type :json,
             :idx ["my:json"]}}

           (expand/node {"gr:includes"               {"@type"     ["gr:Individual" "pto:Vehicle"],
                                                      "gr:name"   "Tesla Roadster",
                                                      "foaf:page" "http://www.teslamotors.com/roadster"},
                         "@context"                  {"gr"                        "http://purl.org/goodrelations/v1#",
                                                      "pto"                       "http://www.productontology.org/id/",
                                                      "foaf"                      "http://xmlns.com/foaf/0.1/",
                                                      "xsd"                       "http://www.w3.org/2001/XMLSchema#",
                                                      "foaf:page"                 {"@type" "@id"},
                                                      "gr:acceptedPaymentMethods" {"@type" "@id"},
                                                      "gr:hasBusinessFunction"    {"@type" "@id"},
                                                      "gr:hasCurrencyValue"       {"@type" "xsd:float"}
                                                      "my:json" {"@type" "@json"}},
                         "@id"                       "http://example.org/cars/for-sale#tesla",
                         "gr:description"            "Need to sell fast and furiously",
                         "gr:name"                   "Used Tesla Roadster",
                         "gr:hasPriceSpecification"  {"gr:hasCurrencyValue" "85000", "gr:hasCurrency" "USD"},
                         "gr:acceptedPaymentMethods" "gr:Cash",
                         "gr:hasBusinessFunction"    "gr:Sell",
                         "my:json"                   {"foo" {:bar [1 false 9.0 nil]}}
                         "@type"                     "gr:Offering"}))))


  (testing "Nested children in vector"
    (is (= {"http://rdf.data-vocabulary.org/#name"
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
               :value "Garnish with a lime wedge."}}]
            :idx []}
           (expand/node {"@context"     {"name"         "http://rdf.data-vocabulary.org/#name",
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
                                         {"step" 5, "description" "Garnish with a lime wedge."}]})))))

(deftest node-graph-parse
  (testing "Parse node that is a graph"
    (is (= [{:idx  ["@graph" 0],
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
              :idx   ["@graph" 2 "dc11:title"]}}]
           (into []
                 (expand/node {"@context" {"dc11"        "http://purl.org/dc/elements/1.1/",
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
                                            "dc11:title"       "The Introduction"}]}))))))


(deftest list-type-values
  (testing "An @list can be used in context to specify all instances of the node are list items"
    (is (= {:id  "http://example.org/people#joebob"
            :idx []
            "http://xmlns.com/foaf/0.1/nick"
            {:list [{:value "joe", :type nil, :idx ["nick" 0]}
                    {:value "bob", :type nil, :idx ["nick" 1]}
                    {:value "jaybee", :type nil, :idx ["nick" 2]}]}}
           (expand/node {"@context" {"nick" {"@id"        "http://xmlns.com/foaf/0.1/nick",
                                             "@container" "@list"}}

                         "@id"  "http://example.org/people#joebob",
                         "nick" ["joe", "bob", "jaybee"]}))))
  (testing "An @list can also be used within the node itself"
    (is (= {:id  "http://example.org/people#joebob"
            :idx []
            "http://xmlns.com/foaf/0.1/nick"
            {:list [{:value "joe", :type nil, :idx ["foaf:nick" "@list" 0]}
                    {:value "bob", :type nil, :idx ["foaf:nick" "@list" 1]}
                    {:value "jaybee", :type nil, :idx ["foaf:nick" "@list" 2]}]}}
           (expand/node {"@context"  {"foaf" "http://xmlns.com/foaf/0.1/"}
                         "@id"       "http://example.org/people#joebob",
                         "foaf:nick" {"@list" ["joe", "bob", "jaybee"]}})))))

(deftest set-type-values
  (testing "An @set is the default list/vector container type, even if specified, flatten."
    (is (= {:id  "http://example.org/people#joebob"
            :idx []
            "http://xmlns.com/foaf/0.1/nick"
            [{:value "joe", :type nil, :idx ["foaf:nick" "@set" 0]}
             {:value "bob", :type nil, :idx ["foaf:nick" "@set" 1]}
             {:value "jaybee", :type nil, :idx ["foaf:nick" "@set" 2]}]}
           (expand/node {"@context"  {"foaf" "http://xmlns.com/foaf/0.1/"}
                         "@id"       "http://example.org/people#joebob",
                         "foaf:nick" {"@set" ["joe", "bob", "jaybee"]}}))))
  (testing "An @set can be used in context ensure all values presented in vector/array format."
    (is (= {:id  "http://example.org/people#joebob"
            :idx []
            "http://xmlns.com/foaf/0.1/nick"
            [{:value "joe", :type nil, :idx ["nick" 0]}
             {:value "bob", :type nil, :idx ["nick" 1]}
             {:value "jaybee", :type nil, :idx ["nick" 2]}]}
           (expand/node {"@context" {"nick" {"@id"        "http://xmlns.com/foaf/0.1/nick",
                                             "@container" "@set"}}

                         "@id"  "http://example.org/people#joebob",
                         "nick" ["joe", "bob", "jaybee"]})))))


(deftest base-and-vocab-test
  (testing "An @base and a @vocab expand the correct iris"
    (is (= {:id   "https://base.com/base/iri#joebob"
            :idx  []
            :type ["https://vocab.com/vocab/iri/Joey"]
            "https://vocab.com/vocab/iri/name"
            {:value "Joe Bob" :type nil :idx ["name"]}
            "https://vocab.com/vocab/iri/iriProperty"
            {:id  "https://base.com/base/iri#a-relative-id"
             :idx ["iriProperty"]}}
           (expand/node {"@context"    {"@base"       "https://base.com/base/iri"
                                        "@vocab"      "https://vocab.com/vocab/iri/"
                                        "iriProperty" {"@type" "@id"}}
                         "@id"         "#joebob",
                         "@type"       "Joey"
                         "name"        "Joe Bob"
                         "iriProperty" "#a-relative-id"}))))
  (testing "A absolute @base and a relative @vocab expand the correct iris"
    (let [context* {"@base"       "http://example.com/"
                    "@vocab"      "ns/"
                    "iriProperty" {"@type" "@id"}}
          data     {"@id"         "#joebob"
                    "@type"       "Joey"
                    "name"        "Joe Bob"
                    "iriProperty" "#a-relative-id"}]
      (is (= {:idx                                []
              :type                               ["http://example.com/ns/Joey"]
              :id                                 "http://example.com/#joebob"
              "http://example.com/ns/name"        {:value "Joe Bob"
                                                   :type  nil
                                                   :idx   ["name"]}
              "http://example.com/ns/iriProperty" {:id  "http://example.com/#a-relative-id"
                                                   :idx ["iriProperty"]}}
             (expand/node (assoc data "@context" context*)))))))

(deftest type-sub-context
  (testing "When @context has a sub-@context for specific types, ensure merged"
    (let [expected {:id "#joebob"
                    :idx []
                    :type ["https://www.w3.org/2018/credentials#VerifiableCredential"]
                    "https://www.w3.org/2018/credentials#issuer"
                    {:id "did:for:some-issuer" :idx ["issuer"]}
                    "https://www.w3.org/2018/credentials#credentialSchema"
                    {:id "#credSchema"
                     :idx ["credentialSchema"]
                     "https://www.w3.org/2018/credentials#" {:value "Some Cred!"
                                                             :type nil
                                                             :idx ["credentialSchema" "cred"]}}}]
      ;; expect same output independent of key order of input
      ;; "type" is 3rd key
      (is (= expected
             (expand/node {"@context" {"id" "@id",
                                       "type" "@type",
                                       "VerifiableCredential" {"@id" "https://www.w3.org/2018/credentials#VerifiableCredential",
                                                               "@context" {"id" "@id",
                                                                           "type" "@type",
                                                                           "cred" "https://www.w3.org/2018/credentials#",
                                                                           "sec" "https://w3id.org/security#",
                                                                           "xsd" "http://www.w3.org/2001/XMLSchema#",
                                                                           "credentialSchema" {"@id" "cred:credentialSchema",
                                                                                               "@type" "@id",
                                                                                               "@context" {"id" "@id",
                                                                                                           "type" "@type",
                                                                                                           "cred" "https://www.w3.org/2018/credentials#",
                                                                                                           "JsonSchemaValidator2018" "cred:JsonSchemaValidator2018"}},
                                                                           "issuer" {"@id" "cred:issuer"
                                                                                     "@type" "@id"}}}}
                           "id" "#joebob",
                           "type" ["VerifiableCredential"]
                           "issuer" "did:for:some-issuer"
                           "credentialSchema" {"id" "#credSchema"
                                               "cred" "Some Cred!"}})))

      ;; "type" is 5th key
      (is (= expected
             (expand/node {"@context" {"id" "@id",
                                       "type" "@type",
                                       "VerifiableCredential" {"@id" "https://www.w3.org/2018/credentials#VerifiableCredential",
                                                               "@context" {"id" "@id",
                                                                           "type" "@type",
                                                                           "cred" "https://www.w3.org/2018/credentials#",
                                                                           "sec" "https://w3id.org/security#",
                                                                           "xsd" "http://www.w3.org/2001/XMLSchema#",
                                                                           "credentialSchema" {"@id" "cred:credentialSchema",
                                                                                               "@type" "@id",
                                                                                               "@context" {"id" "@id",
                                                                                                           "type" "@type",
                                                                                                           "cred" "https://www.w3.org/2018/credentials#",
                                                                                                           "JsonSchemaValidator2018" "cred:JsonSchemaValidator2018"}},
                                                                           "issuer" {"@id" "cred:issuer"
                                                                                     "@type" "@id"}}}}
                           "id" "#joebob",
                           "issuer" "did:for:some-issuer"
                           "credentialSchema" {"id" "#credSchema"
                                               "cred" "Some Cred!"}
                           "type" ["VerifiableCredential"]}))))))

(deftest keyword-contexts
  (testing "Clojure keyword contexts"
    (is (= (expand/node {:context     {:id     "@id"
                                       :type   "@type"
                                       :schema "http://schema.org/"}
                         :id          "http://example.com/ns#item123"
                         :type        :schema/Movie
                         :schema/name "My Movie"})
           {:idx                     [],
            :type                    ["http://schema.org/Movie"],
            :id                      "http://example.com/ns#item123",
            "http://schema.org/name" {:value "My Movie", :type nil, :idx [:schema/name]}})))
  (testing "Clojure keyword used as IRI value"
    (is (= (expand/node {:context     {:id     "@id"
                                       :type   "@type"
                                       :schema "http://schema.org/"
                                       :ex     "http://example.com/ns#"}
                         :id          :ex/item123
                         :type        :schema/Movie
                         :schema/name "My Movie"})
           {:idx                     [],
            :type                    ["http://schema.org/Movie"],
            :id                      "http://example.com/ns#item123",
            "http://schema.org/name" {:value "My Movie", :type nil, :idx [:schema/name]}})))
  (testing "Clojure keyword used as IRI value in a vector"
    (is (= (expand/node {:context     {:id     "@id"
                                       :ex     "http://example.com/ns#"}
                         :id          :ex/item123
                         :ex/favColor [:ex/red :ex/green]})
           {:idx [],
            :id "http://example.com/ns#item123",
            "http://example.com/ns#favColor" [{:id "http://example.com/ns#red", :idx [:ex/favColor 0]}
                                              {:id "http://example.com/ns#green", :idx [:ex/favColor 1]}]}))))

(deftest shacl-embedded-nodes
    (testing "clojure kws"
      (is (= {:idx [],
              :type ["http://www.w3.org/ns/shacl#NodeShape"],
              :id "http://example.org/ns/UserShape",
              "http://www.w3.org/ns/shacl#targetClass"
              {:id "http://example.org/ns/User",
               :idx [:sh/targetClass]},
              "http://www.w3.org/ns/shacl#property"
              [{:idx [:sh/property 0],
                "http://www.w3.org/ns/shacl#path"
                {:id "http://schema.org/name",
                 :idx [:sh/property 0 :sh/path]},
                "http://www.w3.org/ns/shacl#datatype"
                {:id "http://www.w3.org/2001/XMLSchema#string",
                 :idx [:sh/property 0 :sh/datatype]}}]}

             (expand/node
              {:id :ex/UserShape,
               :type [:sh/NodeShape],
               :sh/targetClass :ex/User,
               :sh/property [{:sh/path :schema/name,
                              :sh/datatype :xsd/string}]}

              {:schema {:id "http://schema.org/"},
               :wiki {:id "https://www.wikidata.org/wiki/"},
               :xsd {:id "http://www.w3.org/2001/XMLSchema#"},
               :type {:id "@type",
                      :type? true},
               :rdfs {:id "http://www.w3.org/2000/01/rdf-schema#"},
               :type-key :type,
               :ex {:id "http://example.org/ns/"},
               :id {:id "@id"},
               :f {:id "https://ns.flur.ee/ledger#"},
               :sh {:id "http://www.w3.org/ns/shacl#"},
               :skos {:id "http://www.w3.org/2008/05/skos#"},
               :rdf {:id "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}})){}))
    (testing "string, with `:type-key` of 'type', but node uses @type"
      (is (= {:idx [],
              :type ["http://www.w3.org/ns/shacl#NodeShape"],
              :id "http://example.org/ns/UserShape",
              "http://www.w3.org/ns/shacl#targetClass"
              {:id "http://example.org/ns/User",
               :idx ["sh:targetClass"]},
              "http://www.w3.org/ns/shacl#property"
              [{:idx ["sh:property" 0],
                "http://www.w3.org/ns/shacl#path"
                {:id "http://schema.org/name",
                 :idx ["sh:property" 0 "sh:path"]},
                "http://www.w3.org/ns/shacl#datatype"
                {:id "http://www.w3.org/2001/XMLSchema#string",
                 :idx ["sh:property" 0 "sh:datatype"]}}]}

             (expand/node
              {"@id" "ex:UserShape",
               "@type" ["sh:NodeShape"],
               "sh:targetClass" {"@id" "ex:User"},
               "sh:property" [{"sh:datatype" {"@id" "xsd:string"},
                               "sh:path" {"@id" "schema:name"}}]}

              {"f" {:id "https://ns.flur.ee/ledger#"},
               "rdf" {:id "http://www.w3.org/1999/02/22-rdf-syntax-ns#"},
               "schema" {:id "http://schema.org/"},
               "id" {:id "@id"},
               "wiki" {:id "https://www.wikidata.org/wiki/"},
               :type-key "type",
               "ex" {:id "http://example.org/ns/"},
               "rdfs" {:id "http://www.w3.org/2000/01/rdf-schema#"},
               "type" {:id "@type",
                       :type? true},
               "sh" {:id "http://www.w3.org/ns/shacl#"},
               "skos" {:id "http://www.w3.org/2008/05/skos#"},
               "xsd" {:id "http://www.w3.org/2001/XMLSchema#"}})))))

(deftest language-tag-test
  (testing "expanding nodes with language tags"
    (testing "in value maps"
      (testing "with no specified type"
        (let [jsonld {"@context"      {"ex" "http://example.com/vocab/"}
                      "ex:name"       "Frank"
                      "ex:occupation" {"@value"    "Ninja"
                                       "@language" "en"}}]
          (is (= {:idx                            [],
                  "http://example.com/vocab/name" {:value "Frank", :type nil, :idx ["ex:name"]},
                  "http://example.com/vocab/occupation"
                  {:value    "Ninja"
                   :language "en"
                   :idx      ["ex:occupation"]}}
                 (expand/node jsonld))
              "includes the language tag")))
      (testing "with a type specified"
        (let [jsonld {"@context"      {"ex"  "http://example.com/vocab/"
                                       "xsd" "http://www.w3.org/2001/XMLSchema#"}
                      "ex:name"       "Frank"
                      "ex:occupation" {"@value"    "Ninja"
                                       "@type"     "xsd:string"
                                       "@language" "en"}}]
          (is (thrown-with-msg? ExceptionInfo
                                #"@language cannot be used for values with a specified @type"
                                (expand/node jsonld))
              "throws an exception indicating an invalid type"))))
    (testing "in the context"
      (let [jsonld {"@context"      {"ex"        "http://example.com/vocab/"
                                     "@language" "en"}
                    "ex:name"       "Frank"
                    "ex:age"        33
                    "ex:occupation" {"@value" "Ninja"}}]
        (is (= {:idx [],
                "http://example.com/vocab/name"
                {:value "Frank", :language "en", :idx ["ex:name"]}
                "http://example.com/vocab/age"
                {:value 33, :type nil, :idx ["ex:age"]}
                "http://example.com/vocab/occupation"
                {:value "Ninja", :language "en", :idx ["ex:occupation"]}}
               (expand/node jsonld))
            "includes the language tag for all string values")
        (testing "cleared in an intervening context"
          (let [jsonld* (-> jsonld
                            (assoc-in ["@context" "ex:details" "@context"]
                                      {"@language" nil})
                            (dissoc "ex:occupation")
                            (assoc-in ["ex:details" "ex:occupation"] "Ninja"))]
            (is (= {:idx [],
                    "http://example.com/vocab/name"
                    {:value "Frank", :language "en", :idx ["ex:name"]},
                    "http://example.com/vocab/age"
                    {:value 33, :type nil, :idx ["ex:age"]},
                    "http://example.com/vocab/details"
                    {:idx ["ex:details"],
                     "http://example.com/vocab/occupation"
                     {:value "Ninja", :type nil, :idx ["ex:details" "ex:occupation"]}}}
                   (expand/node jsonld*))
                "does not include the language tag")))))))

(comment
  (expanding-iri)
  (expanding-reverse-iri)
  (expanding-node)
  (node-graph-parse)
  (list-type-values)
  (set-type-values)
  (base-and-vocab-test)
  (type-sub-context)
  (keyword-contexts)

  ,)
