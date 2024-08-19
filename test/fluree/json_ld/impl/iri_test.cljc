(ns fluree.json-ld.impl.iri-test
  (:require #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [fluree.json-ld.impl.iri :as iri]))

(deftest prefix-parsing
  (testing "Prefix parsing returns prefix and suffix correctly"

    (is (= (iri/parse-prefix "schema:name") ["schema" "name"]))

    (is (= (iri/parse-prefix "fluree:some/namespace") ["fluree" "some/namespace"]))

    (is (= (iri/parse-prefix "ex:a") ["ex" "a"]))

    (is (= (iri/parse-prefix "a:b") ["a" "b"]))

    (is (nil? (iri/parse-prefix "fluree/some:namespace")))

    (is (nil? (iri/parse-prefix "fluree/cool")))

    (is (nil? (iri/parse-prefix "schema::name")))

    (is (= (iri/parse-prefix ":schema:name") [":" "schema:name"]))

    (is (= (iri/parse-prefix ":schema") [":" "schema"]))

    (is (nil? (iri/parse-prefix "schema:")))

    (is (nil? (iri/parse-prefix "schema:/name")))

    (is (nil? (iri/parse-prefix "schema:name:")))

    (is (nil? (iri/parse-prefix "https://schema.org")))

    (is (nil? (iri/parse-prefix "https://schema.org:bad")))

    (is (nil? (iri/parse-prefix "schema-name")))))
