(ns com.fluree.json-ld-test
  (:require [clojure.test :refer :all]
            [com.fluree.json-ld :refer :all]
            [clojure.java.io :as io])
  (:import (com.apicatalog.jsonld.api.impl ExpansionApi)))

(deftest expansion-test
  (testing "creates an ExpansionApi instance from a JSON-LD file"
    (let [json-ld (.toString (io/resource "test1.jsonld"))
          expansion (expand json-ld)]
      (is (instance? ExpansionApi expansion))))

  (testing "expands to the expected JSON"
    (let [json-ld (.toString (io/resource "test1.jsonld"))
          expansion (expand json-ld)]
      (is (= "[{\"@id\":\"http://dbpedia.org/resource/John_Lennon\",\"http://xmlns.com/foaf/0.1/name\":[{\"@value\":\"John Lennon\"}],\"http://schema.org/birthDate\":[{\"@value\":\"1940-10-09\",\"@type\":\"http://www.w3.org/2001/XMLSchema#date\"}],\"http://schema.org/spouse\":[{\"@id\":\"http://dbpedia.org/resource/Cynthia_Lennon\"}]}]"
             (.toString (expansion->json expansion)))))))
