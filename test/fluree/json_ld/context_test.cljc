(ns fluree.json-ld.context-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld.context :as context]))


(deftest default-vocabularies
  (testing "An explicitly defined default vocabulary with @vocab"
    (is (= (context/parse {"@vocab" "https://schema.org/"})
           {:type-key "@type"
            :vocab    "https://schema.org/"})))

  (testing "References to default vocabulary should be concatenated"
    (is (= (context/parse {"@vocab"     "https://schema.org/"
                           "reverseRef" {"@reverse" "isBasedOn"}
                           "explicit"   "name"
                           "dontTouch"  "https://example.com/ns#42"
                           "id"         "@id"})
           {:type-key    "@type"
            :vocab       "https://schema.org/"
            "reverseRef" {:reverse "https://schema.org/isBasedOn"}
            "explicit"   {:id "https://schema.org/name"}
            "dontTouch"  {:id "https://example.com/ns#42"}
            "id"         {:id "@id"}}))))


(deftest map-context-parsing
  (testing "Context map parsing"

    ;; string based context
    (is (= (context/parse {"owl" "http://www.w3.org/2002/07/owl#",
                           "ex"  "http://example.org/ns#"})
           {:type-key "@type"
            "owl"     {:id "http://www.w3.org/2002/07/owl#"}
            "ex"      {:id "http://example.org/ns#"}}))


    ;; keywords are retained
    (is (= (context/parse {:owl "http://www.w3.org/2002/07/owl#",
                           :ex  "http://example.org/ns#"})
           {:type-key "@type"
            :owl      {:id "http://www.w3.org/2002/07/owl#"}
            :ex       {:id "http://example.org/ns#"}}))))


(deftest dependent-context
  (testing "Some contexts use compact IRIs defined in their own document"
    (testing "One level deep"
      (is (= (context/parse {"nc"   "http://release.niem.gov/niem/niem-core/4.0/#",
                             "name" "nc:PersonName"})
             {:type-key "@type"
              "nc"      {:id "http://release.niem.gov/niem/niem-core/4.0/#"}
              "name"    {:id "http://release.niem.gov/niem/niem-core/4.0/#PersonName"}})))
    (testing "Two levels deep"
      ;; from CLR vocabulary
      (is (= (context/parse {"clri"      "https://purl.imsglobal.org/spec/clr/vocab#"
                             "Address"   "dtAddress",
                             "dtAddress" "clri:dtAddress"})
             {:type-key   "@type"
              "Address"   {:id "https://purl.imsglobal.org/spec/clr/vocab#dtAddress"}
              "clri"      {:id "https://purl.imsglobal.org/spec/clr/vocab#"}
              "dtAddress" {:id "https://purl.imsglobal.org/spec/clr/vocab#dtAddress"}})))
    (testing "Two levels deep with map val"
      ;; from CLR vocabulary
      (is (= (context/parse {"clri"   "https://purl.imsglobal.org/spec/clr/vocab#"
                             "xsd"    "http://www.w3.org/2001/XMLSchema#"
                             "UUID"   "dtUUID"
                             "dtUUID" {"@id"   "clri:dtUUID",
                                       "@type" "xsd:string"}})
             {:type-key "@type"
              "UUID"    {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
                         :type "http://www.w3.org/2001/XMLSchema#string"}
              "clri"    {:id "https://purl.imsglobal.org/spec/clr/vocab#"}
              "dtUUID"  {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
                         :type "http://www.w3.org/2001/XMLSchema#string"}
              "xsd"     {:id "http://www.w3.org/2001/XMLSchema#"}})))))


(deftest multiple-contexts
  (testing "Context map parsing"
    (is (= (context/parse [{"schema" "http://schema.org/"},
                           {"owl" "http://www.w3.org/2002/07/owl#",
                            "ex"  "http://example.org/ns#"}])
           {:type-key "@type"
            "schema"  {:id "http://schema.org/"}
            "owl"     {:id "http://www.w3.org/2002/07/owl#"}
            "ex"      {:id "http://example.org/ns#"}})))

  (testing "An second context may rely on definitions in the first"
    ;; this scenario happened with https://w3id.org/security/v1 -> https://w3id.org/security/v2
    (is (= (context/parse [{"sec" "https://w3id.org/security#"}
                           {"EcdsaSecp256k1VerificationKey2019" "sec:EcdsaSecp256k1VerificationKey2019"}])
           {:type-key                           "@type"
            "sec"                               {:id "https://w3id.org/security#"},
            "EcdsaSecp256k1VerificationKey2019" {:id "https://w3id.org/security#EcdsaSecp256k1VerificationKey2019"}}))))


(deftest nested-context-details
  (testing "Context keys with map values."

    ;; custom full iri with type defined
    (is (= (context/parse {"schema"       "http://schema.org/",
                           "customScalar" {"@id"   "http://schema.org/name"
                                           "@type" "http://schema.org/Text"}})
           {:type-key      "@type"
            "schema"       {:id "http://schema.org/"}
            "customScalar" {:id   "http://schema.org/name"
                            :type "http://schema.org/Text"}}))

    (is (= (context/parse {"schema"      "http://schema.org/",
                           "customClass" {"@id"   "schema:Book"
                                          "@type" "schema:CreativeWork"}})
           {:type-key     "@type"
            "schema"      {:id "http://schema.org/"}
            "customClass" {:id   "http://schema.org/Book"
                           :type "http://schema.org/CreativeWork"}}))))


(deftest reverse-refs
  (testing "Reverse refs using full IRI"
    (is (= (context/parse {"@vocab"       "http://schema.org/"
                           "title"        "http://schema.org/titleEIDR"
                           "derivedWorks" {"@reverse" "http://schema.org/isBasedOn"}})
           {:type-key      "@type"
            :vocab         "http://schema.org/",
            "title"        {:id "http://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "http://schema.org/isBasedOn"}})))

  (testing "Reverse refs with compact-iri"
    (is (= (context/parse {"schema"       "http://schema.org/"
                           "title"        "schema:titleEIDR"
                           "derivedWorks" {"@reverse" "schema:isBasedOn"}})
           {:type-key      "@type"
            "schema"       {:id "http://schema.org/"},
            "title"        {:id "http://schema.org/titleEIDR"},
            "derivedWorks" {:reverse "http://schema.org/isBasedOn"}}))))


(deftest type-only
  (testing "A context map's value can include only @type and we must infer @id"
    (is (= (context/parse {"ical"         "http://www.w3.org/2002/12/cal/ical#",
                           "xsd"          "http://www.w3.org/2001/XMLSchema#",
                           "ical:dtstart" {"@type" "xsd:dateTime"}})
           {:type-key      "@type"
            "ical"         {:id "http://www.w3.org/2002/12/cal/ical#"},
            "xsd"          {:id "http://www.w3.org/2001/XMLSchema#"},
            "ical:dtstart" {:type "http://www.w3.org/2001/XMLSchema#dateTime",
                            :id   "http://www.w3.org/2002/12/cal/ical#dtstart"}}))))

(deftest blank-vocab
  (testing "An empty string @vocab should default to @base value."
    (is (= (context/parse {"@base"  "https://hub.flur.ee/some/ledger/"
                           "@vocab" ""})
           {:type-key "@type"
            :base     "https://hub.flur.ee/some/ledger/"
            :vocab    "https://hub.flur.ee/some/ledger/"}))))

(deftest metadata-parses
  (testing "@protected and @version properly parses"
    (is (= (context/parse {"@version"   1.1,
                           "@protected" true
                           "schema"     "http://schema.org/"})
           {:type-key  "@type"
            :version   1.1
            :protected true
            "schema"   {:id "http://schema.org/"}}))))

(deftest containers-parse
  (testing "An @container value properly parses"
    (is (= (context/parse {"schema" "http://schema.org/"
                           "post"   {"@id"        "schema:blogPost",
                                     "@container" "@set"}})
           {:type-key "@type"
            "schema"  {:id "http://schema.org/"}
            "post"    {:id        "http://schema.org/blogPost"
                       :container :set}})))
  (testing "Multiple @container values are allowed"
    (is (= (context/parse {"schema" "http://schema.org/"
                           "post"   {"@id"        "schema:blogPost",
                                     "@container" ["@index" "@set"]}})
           {:type-key "@type"
            "schema"  {:id "http://schema.org/"}
            "post"    {:id        "http://schema.org/blogPost"
                       :container [:index :set]}}))))

(deftest keyword-context
  (testing "Using Clojure keywords in contexts"
    (is (= (context/parse {:schema "http://schema.org/"})
           {:type-key "@type"
            :schema   {:id "http://schema.org/"}}))

    (is (= (context/parse {:vocab "http://schema.org/"})
           {:type-key "@type"
            :vocab    {:id "http://schema.org/"}}))

    (is (= (context/parse {:id     "@id"
                           :type   "@type"
                           :schema "http://schema.org/"})
           {:type-key :type
            :id       {:id "@id"}
            :type     {:id "@type", :type? true}
            :schema   {:id "http://schema.org/"}}))))
