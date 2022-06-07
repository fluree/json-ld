(ns fluree.json-ld.impl.canonicalize-test
  (:require  [clojure.test :as t]
             [cheshire.core :as json]
             [fluree.json-ld.impl.nquads :as nquads]
             [fluree.json-ld.impl.canonicalize :as canon]
             [clojure.java.io :as io]
             [clojure.string :as str]))

(defn read-file
  "Read a file from the rdf-canonicalization directory."
  [path]
  (slurp (io/resource (str "rdf-canonicalization/" path))))

(def tests
  "Loads all the test data into a map of test-id->test-def."
  (let [manifest (json/parse-string (read-file "manifest-urdna2015.jsonld") )
        test-defs (get manifest "entries")]
    (-> manifest
        (assoc :test-ids (map #(get % "id") test-defs))
        (assoc :test-id->test-def (reduce (fn [m test-def] (assoc m (get test-def "id") test-def))
                                          {}
                                          test-defs)))))

(defn get-test-def
  [test-id]
  (-> tests :test-id->test-def (get test-id)))

(defn run-test
  [test-def]
  (let [{test-name "name" id "id"  input-path "action" expect-path "result" type "type"}
        test-def
        input (when input-path (read-file input-path))
        expect (when expect-path (read-file expect-path))]
    (t/testing (str test-name ":")
      (t/testing id
        (println "Testing" test-name id (pr-str input))
        (t/is (= expect
                 (canon/canonicalize (nquads/parse input))))))))

(t/deftest rdf-canonicalization-test
  (doseq [test-id (:test-ids tests)]
    (run-test (get-test-def test-id))))


(comment
  (t/deftest individual-test
    (run-test (get-test-def "manifest-urdna2015#test020")))

  (run-test (get-test-def "manifest-urdna2015#test019"))

  (count (:test-ids tests))

  (first (get tests "entries"))
  {"id" "manifest-urdna2015#test001",
   "type" "rdfn:Urdna2015EvalTest",
   "name" "simple id",
   "comment" nil,
   "approval" "rdft:Proposed",
   "action" "test001-in.nq",
   "result" "test001-urdna2015.nq"}




  )
