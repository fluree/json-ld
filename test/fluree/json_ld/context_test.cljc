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
           {:vocab "https://schema.org/"})))

  (testing "References to default vocabulary should be concatenated"
    (is (= (parse {"@vocab"     "https://schema.org/"
                   "reverseRef" {"@reverse" "isBasedOn"}
                   "explicit"   "name"
                   "dontTouch"  "https://example.com/ns#42"
                   "id"         "@id"})
           {:vocab       "https://schema.org/"
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
    (testing "One level deep"
      (is (= (parse {"nc"   "http://release.niem.gov/niem/niem-core/4.0/#",
                     "name" "nc:PersonName"})
             {"nc"   {:id "http://release.niem.gov/niem/niem-core/4.0/#"}
              "name" {:id "http://release.niem.gov/niem/niem-core/4.0/#PersonName"}})))
    (testing "Two levels deep"
      ;; from CLR vocabulary
      (is (= (parse {"clri"      "https://purl.imsglobal.org/spec/clr/vocab#"
                     "Address"   "dtAddress",
                     "dtAddress" "clri:dtAddress"})
             {"Address"   {:id "https://purl.imsglobal.org/spec/clr/vocab#dtAddress"}
              "clri"      {:id "https://purl.imsglobal.org/spec/clr/vocab#"}
              "dtAddress" {:id "https://purl.imsglobal.org/spec/clr/vocab#dtAddress"}})))
    (testing "Two levels deep with map val"
      ;; from CLR vocabulary
      (is (= (parse {"clri"   "https://purl.imsglobal.org/spec/clr/vocab#"
                     "xsd"    "http://www.w3.org/2001/XMLSchema#"
                     "UUID"   "dtUUID"
                     "dtUUID" {"@id"   "clri:dtUUID",
                               "@type" "xsd:string"}})
             {"UUID"   {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
                        :type "http://www.w3.org/2001/XMLSchema#string"}
              "clri"   {:id "https://purl.imsglobal.org/spec/clr/vocab#"}
              "dtUUID" {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
                        :type "http://www.w3.org/2001/XMLSchema#string"}
              "xsd"    {:id "http://www.w3.org/2001/XMLSchema#"}})))))


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
            "ex"     {:id "http://example.org/ns#"}})))

  (testing "An second context may rely on definitions in the first"
    ;; this scenario happened with https://w3id.org/security/v1 -> https://w3id.org/security/v2
    (is (= (parse [{"sec" "https://w3id.org/security#"}
                   {"EcdsaSecp256k1VerificationKey2019" "sec:EcdsaSecp256k1VerificationKey2019"}])
           {"sec"                               {:id "https://w3id.org/security#"},
            "EcdsaSecp256k1VerificationKey2019" {:id "https://w3id.org/security#EcdsaSecp256k1VerificationKey2019"}}))))


(deftest nested-context-details
  (testing "Context keys with map values."

    ;; custom full iri with type defined
    (is (= (parse {"schema"       "https://schema.org/",
                   "customScalar" {"@id"   "https://schema.org/name"
                                   "@type" "https://schema.org/Text"}})
           {"schema"       {:id "https://schema.org/"}
            "customScalar" {:id   "https://schema.org/name"
                            :type "https://schema.org/Text"}}))

    (is (= (parse {"schema"      "https://schema.org/",
                   "customClass" {"@id"   "schema:Book"
                                  "@type" "schema:CreativeWork"}})
           {"schema"      {:id "https://schema.org/"}
            "customClass" {:id   "https://schema.org/Book"
                           :type "https://schema.org/CreativeWork"}}))))


(deftest reverse-refs
  (testing "Reverse refs using full IRI"
    (is (= (parse {"@vocab"       "https://schema.org/"
                   "title"        "https://schema.org/titleEIDR"
                   "derivedWorks" {"@reverse" "https://schema.org/isBasedOn"}})
           {:vocab         "https://schema.org/",
            "title"        {:id "https://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "https://schema.org/isBasedOn"}})))

  (testing "Reverse refs with compact-iri"
    (is (= (parse {"schema"       "https://schema.org/"
                   "title"        "schema:titleEIDR"
                   "derivedWorks" {"@reverse" "schema:isBasedOn"}})
           {"schema"       {:id "https://schema.org/"},
            "title"        {:id "https://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "https://schema.org/isBasedOn"}}))))


(deftest type-only
  (testing "A context map's value can include only @type and we must infer @id"
    (is (= (parse {"ical"         "http://www.w3.org/2002/12/cal/ical#",
                   "xsd"          "http://www.w3.org/2001/XMLSchema#",
                   "ical:dtstart" {"@type" "xsd:dateTime"}})
           {"ical"         {:id "http://www.w3.org/2002/12/cal/ical#"},
            "xsd"          {:id "http://www.w3.org/2001/XMLSchema#"},
            "ical:dtstart" {:type "http://www.w3.org/2001/XMLSchema#dateTime",
                            :id   "http://www.w3.org/2002/12/cal/ical#dtstart"}}))))

(deftest blank-vocab
  (testing "An empty string @vocab should default to @base value."
    (is (= (parse {"@base"  "https://hub.flur.ee/some/ledger/"
                   "@vocab" ""})
           {:base  "https://hub.flur.ee/some/ledger/"
            :vocab "https://hub.flur.ee/some/ledger/"}))))

(deftest metadata-parses
  (testing "@protected and @version properly parses"
    (is (= (parse {"@version"   1.1,
                   "@protected" true
                   "schema"     "http://schema.org/"})
           {:version   1.1
            :protected true
            "schema"   {:id "http://schema.org/"}}))))

(deftest containers-parse
  (testing "An @container value properly parses"
    (is (= (parse {"schema" "http://schema.org/"
                   "post"   {"@id"        "schema:blogPost",
                             "@container" "@set"}})
           {"schema" {:id "http://schema.org/"}
            "post"   {:id        "http://schema.org/blogPost"
                      :container :set}})))
  (testing "Multiple @container values are allowed"
    (is (= (parse {"schema" "http://schema.org/"
                   "post"   {"@id"        "schema:blogPost",
                             "@container" ["@index" "@set"]}})
           {"schema" {:id "http://schema.org/"}
            "post"   {:id        "http://schema.org/blogPost"
                      :container [:index :set]}}))))
