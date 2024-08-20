(ns fluree.json-ld.impl.iri-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld.impl.iri :as iri]))

(deftest prefix-parsing
  (testing "Prefix parsing returns prefix and suffix correctly"

    (is (= ["schema" "name"]
           (iri/parse-prefix "schema:name")))

    (is (= ["fluree" "some/namespace"]
           (iri/parse-prefix "fluree:some/namespace")))

    (is (= ["ex" "a"]
           (iri/parse-prefix "ex:a")))

    (is (= ["a" "b"]
           (iri/parse-prefix "a:b")))

    (is (= [":" "schema:name"]
           (iri/parse-prefix ":schema:name")))

    (is (= [":" "schema"]
           (iri/parse-prefix ":schema")))

    (is (nil? (iri/parse-prefix "fluree/some:namespace")))

    (is (nil? (iri/parse-prefix "fluree/cool")))

    (is (nil? (iri/parse-prefix "schema::name")))

    (is (nil? (iri/parse-prefix "schema:")))

    (is (nil? (iri/parse-prefix "schema:/name")))

    (is (nil? (iri/parse-prefix "schema:name:")))

    (is (nil? (iri/parse-prefix "https://schema.org")))

    (is (nil? (iri/parse-prefix "https://schema.org:bad")))

    (is (nil? (iri/parse-prefix "schema-name")))))
