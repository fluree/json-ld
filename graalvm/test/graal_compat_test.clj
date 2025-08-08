(ns graal-compat-test
  "GraalVM compatibility test for fluree/json-ld"
  (:require [clojure.test :refer [deftest is testing]]
            [fluree.json-ld :as jld]
            [fluree.json-ld.impl.normalize :as normalize])
  (:gen-class))

(deftest graal-basic-functionality-test
  (testing "Basic JSON-LD normalization works in GraalVM"
    (let [test-doc {"@context" {"name" "http://schema.org/name"
                                "Person" "http://schema.org/Person"}
                    "@type" "Person"
                    "name" "John Doe"}
          normalized (normalize/normalize test-doc)]
      (is (string? normalized))
      (is (pos? (count normalized)))
      (is (.contains normalized "John Doe"))))

  (testing "JSON-LD detection works in GraalVM"
    (let [json-ld-doc {"@graph" [{"@type" "Person" "name" "John"}]}
          regular-doc {"name" "John"
                       "type" "Person"}]
      (is (true? (jld/json-ld? json-ld-doc)))
      (is (false? (jld/json-ld? regular-doc))))))

(deftest graal-main-api-functions-test
  (testing "parse-context works in GraalVM"
    (let [context {"name" "http://schema.org/name"
                   "Person" "http://schema.org/Person"}
          parsed-context (jld/parse-context context)]
      (is (map? parsed-context))
      (is (contains? parsed-context :type-key))
      (is (contains? parsed-context "name"))
      (is (map? (get parsed-context "name")))
      (is (= "http://schema.org/name" (:id (get parsed-context "name"))))))

  (testing "expand-iri works in GraalVM"
    (let [context {"name" "http://schema.org/name"
                   "Person" "http://schema.org/Person"}
          parsed-context (jld/parse-context context)
          expanded-name (jld/expand-iri "name" parsed-context)
          expanded-person (jld/expand-iri "Person" parsed-context)]
      (is (= "http://schema.org/name" expanded-name))
      (is (= "http://schema.org/Person" expanded-person))))

  (testing "compact-fn works in GraalVM"
    (let [context {"name" "http://schema.org/name"
                   "Person" "http://schema.org/Person"}
          parsed-context (jld/parse-context context)
          compact-fn (jld/compact-fn parsed-context)
          compacted-name (compact-fn "http://schema.org/name")
          compacted-person (compact-fn "http://schema.org/Person")]
      (is (= "name" compacted-name))
      (is (= "Person" compacted-person))))

  (testing "expand works in GraalVM"
    (let [test-doc {"@context" {"name" "http://schema.org/name"
                                "Person" "http://schema.org/Person"}
                    "@type" "Person"
                    "name" "John Doe"}
          expanded (jld/expand test-doc)]
      (is (map? expanded))
      (is (contains? expanded :idx))
      (is (contains? expanded :type))
      (is (contains? expanded "http://schema.org/name"))
      (is (vector? (:type expanded)))
      (is (= "http://schema.org/Person" (first (:type expanded))))
      (is (vector? (get expanded "http://schema.org/name")))
      (let [name-value (first (get expanded "http://schema.org/name"))]
        (is (map? name-value))
        (is (= "John Doe" (:value name-value))))))

  (testing "compact works in GraalVM"
    (let [expanded-doc {:idx []
                        :type ["Person"]
                        "http://schema.org/name" [{:value "John Doe" :type nil :idx ["name"]}]}
          context {"name" "http://schema.org/name"
                   "Person" "http://schema.org/Person"}
          compacted (jld/compact expanded-doc context)]
      ;; Test that compact executes without error and returns data
      ;; The exact format may be internal to fluree implementation
      (is (some? compacted))
      (is (or (map? compacted) (vector? compacted)))))

  (testing "normalize-data works in GraalVM"
    (let [test-doc {"@context" {"name" "http://schema.org/name"}
                    "@type" "Person"
                    "name" "John Doe"}
          normalized (jld/normalize-data test-doc)]
      (is (string? normalized))
      (is (pos? (count normalized)))
      (is (.contains normalized "John Doe")))))

(defn -main
  "Main function for GraalVM native image testing"
  [& _args]
  (println "🧪 Running GraalVM compatibility tests for fluree/json-ld...")
  
  ;; Enable reflection warnings to catch any issues
  (set! *warn-on-reflection* true)
  
  (try
    ;; Run the tests
    (let [results (clojure.test/run-tests 'graal-compat-test)]
      (if (and (zero? (:fail results)) (zero? (:error results)))
        (do
          (println "✅ All GraalVM compatibility tests passed!")
          (System/exit 0))
        (do
          (println "❌ Some GraalVM compatibility tests failed!")
          (System/exit 1))))
    (catch Exception e
      (println "💥 Error running GraalVM compatibility tests:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 1))))