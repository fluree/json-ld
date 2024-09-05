(ns fluree.json-ld.impl.context-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld.impl.context :as context]))


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
              "xsd"     {:id "http://www.w3.org/2001/XMLSchema#"}
              "dtUUID"  {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
                         :type "http://www.w3.org/2001/XMLSchema#string"}
              "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
              {:id   "https://purl.imsglobal.org/spec/clr/vocab#dtUUID"
               :type "http://www.w3.org/2001/XMLSchema#string"}})))))


(deftest multiple-contexts
  (testing "Context map parsing"
    (is (= {:type-key "@type"
            "schema"  {:id "http://schema.org/"}
            "owl"     {:id "http://www.w3.org/2002/07/owl#"}
            "ex"      {:id "http://example.org/ns#"}}
           (context/parse [{"schema" "http://schema.org/"},
                           {"owl" "http://www.w3.org/2002/07/owl#",
                            "ex"  "http://example.org/ns#"}]))))

  (testing "A second context may rely on definitions in the first"
    ;; this scenario happened with https://w3id.org/security/v1 -> https://w3id.org/security/v2
    (is (= {:type-key                           "@type"
            "sec"                               {:id "https://w3id.org/security#"},
            "EcdsaSecp256k1VerificationKey2019" {:id "https://w3id.org/security#EcdsaSecp256k1VerificationKey2019"}}
           (context/parse [{"sec" "https://w3id.org/security#"}
                           {"EcdsaSecp256k1VerificationKey2019" "sec:EcdsaSecp256k1VerificationKey2019"}]))))
  (testing "A nil context empties the context"
    (let [parsed {:type-key "@type", "sec" {:id "https://w3id.org/security#"}}]
      (is (= parsed
            (context/parse {"sec" "https://w3id.org/security#"}))
          "a parsed context")
      (is (= parsed
             (context/parse parsed {}))
          "an unmodified parsed context")
      (is (= {}
             (context/parse parsed nil))
          "an empty context"))))


(deftest nested-context-details
  (testing "Context keys with map values."

    ;; custom full iri with type defined
    (is (= (context/parse {"schema"       "http://schema.org/",
                           "customScalar" {"@id"   "http://schema.org/name"
                                           "@type" "http://schema.org/Text"}})
           {:type-key      "@type"
            "schema"       {:id "http://schema.org/"}
            "customScalar" {:id   "http://schema.org/name"
                            :type "http://schema.org/Text"}
            "http://schema.org/name"
            {:id   "http://schema.org/name"
             :type "http://schema.org/Text"}}))

    (is (= (context/parse {"schema"      "http://schema.org/",
                           "customClass" {"@id"   "schema:Book"
                                          "@type" "schema:CreativeWork"}})
           {:type-key     "@type"
            "schema"      {:id "http://schema.org/"}
            "customClass" {:id   "http://schema.org/Book"
                           :type "http://schema.org/CreativeWork"}
            "http://schema.org/Book"
            {:id   "http://schema.org/Book"
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
                            :id   "http://www.w3.org/2002/12/cal/ical#dtstart"}
            "http://www.w3.org/2002/12/cal/ical#dtstart"
            {:type "http://www.w3.org/2001/XMLSchema#dateTime"
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

(deftest datatype-full-iri-capture
  (testing "When using a compact IRI to define a default datatype, it should also work for data defined with a full IRI."
    (is (= (context/parse {"ex"        "https://example.com/"
                           "xsd"       "http://www.w3.org/2001/XMLSchema#"
                           "ex:rating" {"@type" "xsd:float"}})
           {:type-key   "@type"
            "ex"        {:id "https://example.com/"}
            "xsd"       {:id "http://www.w3.org/2001/XMLSchema#"}
            "ex:rating" {:id   "https://example.com/rating"
                         :type "http://www.w3.org/2001/XMLSchema#float"}
            "https://example.com/rating"
            {:id   "https://example.com/rating"
             :type "http://www.w3.org/2001/XMLSchema#float"}}))))

#?(:clj
   (deftest cyclic-context
     (testing "compact iri uses itself as a term definition"
       (let [result (try (context/parse {"foo" "foo"})
                         (catch Exception e e))]
         (is (= {:status 400,
                 :error :json-ld/invalid-iri-mapping,
                 :context {"foo" "foo"}}
                (ex-data result)))
         (is (= "A local context contains a term that has an invalid or missing IRI mapping"
                (ex-message result)))))))
