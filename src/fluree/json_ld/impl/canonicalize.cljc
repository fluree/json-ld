(ns fluree.json-ld.impl.canonicalize
  (:require [clojure.string :as str]
            [instaparse.core :as grammar]
            [clojure.java.io :as io]
            [lambdaisland.regal :as reg]
            [lambdaisland.regal.parse :as reg-parse]
            [lambdaisland.regal.generator :as reg-gen]
            [fluree.json-ld.impl.nquads :as nquads]
            [fluree.crypto :as crypto]))

(defn create-issuer
  [prefix]
  {:prefix prefix
   :counter 0
   :issued {}})

(defn issue-id
  [{:keys [prefix counter issued] :as issuer} bnode]
  (if (issued bnode)
    [issuer bnode]
    (let [id (str prefix counter)
          issuer* (-> issuer
                      (update :issued assoc bnode id)
                      (update :counter inc))]
      [issuer* id])))

(defn relabel-bnode
  [reference-bnode node]
  (if (= reference-bnode (:value node))
    (assoc node :value "_:a")
    (assoc node :value "_:z")))

(defn hash-first-degree-quads
  [quads bnode]
  (let [nquads (map (fn [q]
                      (-> q
                          (update :subject (partial relabel-bnode bnode))
                          (update :object (partial relabel-bnode bnode))
                          (update :graph (partial relabel-bnode bnode))
                          (nquads/->statement)))
                    quads)]
    (->> (sort nquads)
         (reduce str)
         (crypto/sha2-256))))

(defn hash-bnode-quads
  [bnode->quads]
  (reduce (fn [hash->bnodes bnode]
            (update hash->bnodes
                    (hash-first-degree-quads bnode->quads bnode)
                    (fnil conj #{})
                    bnode))
          {}
          (keys bnode->quads)))

(defn bnode-quad-info
  "Create a map of bnode ids to the quads that contain them."
  [quads]
  (reduce (fn [bnode->quads {:keys [subject object graph] :as quad}]
            (cond-> bnode->quads
              (= :blank (:type subject)) (update-in [(:value subject) :quads] (fnil conj #{}) quad)
              (= :blank (:type object)) (update-in [(:value object) :quads] (fnil conj #{}) quad)
              (= :blank (:type graph)) (update-in [(:value graph) :quads] (fnil conj #{}) quad)))
          {}
          quads))

(defn aaaa
  [quads]
  (let [bnode->quads (bnode-quad-info quads)
        bnode->quads (reduce-kv (fn [bnode->quads bnode quads]
                                  (assoc-in bnode->quads [bnode :hash] (hash-bnode-quads quads)))
                                {}
                                bnode->quads)]))

(defn hash-related-bnode
  [{:keys [canonical-issuer bnode->quads]} related-bnode quad issuer position]
  (let [id (get (:issued canonical-issuer) related-bnode
                (get (:issued issuer) related-bnode
                     (bnode->quads related-bnode)) )]))

(defn hash-n-degree-quads
  [issuer bnode->quads bnode]
  (let [quads (bnode->quads bnode)]
    (reduce (fn [hash->rel-bnodes quad]
              )
            {}
            quads)))

(defn assign-canonical-ids
  [{:keys [canonical-issuer hash->bnodes bnode->quads] :as canon-state}]
  (let [{:keys [non-uniques canonical-issuer]}
        (->> (keys hash->bnodes)
             (sort)
             (reduce (fn [{:keys [non-uniques canonical-issuer] :as state} hash]
                       (let [bnodes (hash->bnodes hash)]
                         (if (> (count bnodes) 1)
                           (update state :non-uniques conj bnodes)
                           (let [[canonical-issuer* id] (issue-id canonical-issuer (first bnodes))]
                             (assoc state :canonical-issuer canonical-issuer*)))))
                     {:canonical-issuer canonical-issuer
                      :non-uniques #{}}))]
    (def zzz [non-uniques canonical-issuer])
    (map (fn [bnodes]
           (map (fn [bnode]
                  (let [temp-issuer (create-issuer "_:b")
                        [temp-issuer* id] (issue-id temp-issuer bnode)
                        result (hash-n-degree-quads temp-issuer* bnode->quads bnode)])
                  )
                bnodes)
           )
         non-uniques)
    #_(reduce (fn [state bnode]
                (let [[temp-issuer* id] (issue-id (:temp-issuer state) bnode)]
                  (-> state
                      (assoc :temp-issuer temp-issuer*)
                      (update :hash-path-list conj (hash-n-degree-quads temp-issuer* bnode)))))
              {:hash-path-list []
               :temp-issuer temp-issuer}
              non-uniques)))
(update {:a [3]} :a into [1 2])

(defn canonicalize
  [quads]
  (let [canonical-issuer (create-issuer "_:c14n")
        bnode->quads (bnode-quad-info quads)
        hash->bnodes (hash-bnode-quads bnode->quads)
        canon-state {:canonical-issuer canonical-issuer
                     :bnode->quads bnode->quads
                     :hash->bnodes hash->bnodes}

        canonical-ids (assign-canonical-ids canonical-issuer hash->bnodes bnode->quads)]
    canonical-ids))


(comment
  (def in0 "<http://example.com/1> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  (def in1 "<http://example.com/1> <http://example.com/label> \"test\"@en .")
  (def in2 "<http://example.com/1> <http://example.com/friend> _:b1 .")
  (def in3 "<http://example.com/2> <http://example.com/friend> <http://example.com/1> .")
  (def in4 "_:b1 <http://example.com/friend> _:b0 _:b3 .")
  (def in5"<http://example.com/2> <http://example.com/count> \"1\" <http://example.com/graphname> .")
  (def in6 "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  (def in7 "_:b3 <http://example.com/name> \"graph2\" _:b3 .")
  (def in8 "_:b4 <http://example.com/name> _:b5 .")
  (def in9 "_:b5 <http://example.com/name> _:b4 .")
  (def in (str/join "\n" [in0 in1 in2 in3 in4 in5 in6 in7 in8 in9]))


  (canonicalize (nquads/parse in))
  ("_:b5" "_:b4")


  {"c2005112b1fcc34131abf9b1b1f616b0c15cfa6e195119195502a530ffed7b7a" #{"_:b0"},
   "af01ae21de1128b9e9047f01e47a5148925d3e7cabaa47d78bf9e53fd1564413" #{"_:b5" "_:b4"},
   "b09d1d5ad10cd74aeb5838b9c15c9a8d45428f52d5ddc68754c26b88a06c0199" #{"_:b3"},
   "c0d7c06e5bb3c19fbbefbccd225e7e7ac70606331aeed6326c071e45a63e6e1d" #{"_:b1"}}
  {"_:b0"
   #{{:statement
      "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .",
      :subject {:type :blank, :value "_:b0", :term :subject},
      :predicate
      {:type :named,
       :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
       :term :predicate},
      :object
      {:type :named,
       :value "http://example.org/vocab#Foo",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}
     {:statement "_:b1 <http://example.com/friend> _:b0 _:b3 .",
      :subject {:type :blank, :value "_:b1", :term :subject},
      :predicate {:type :named, :value "http://example.com/friend", :term :predicate},
      :object
      {:type :blank,
       :value "_:b0",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :blank, :term :graph, :value "_:b3"}}},
   "_:b4"
   #{{:statement "_:b4 <http://example.com/name> _:b5 .",
      :subject {:type :blank, :value "_:b4", :term :subject},
      :predicate {:type :named, :value "http://example.com/name", :term :predicate},
      :object
      {:type :blank,
       :value "_:b5",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}
     {:statement "_:b5 <http://example.com/name> _:b4 .",
      :subject {:type :blank, :value "_:b5", :term :subject},
      :predicate {:type :named, :value "http://example.com/name", :term :predicate},
      :object
      {:type :blank,
       :value "_:b4",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}},
   "_:b5"
   #{{:statement "_:b4 <http://example.com/name> _:b5 .",
      :subject {:type :blank, :value "_:b4", :term :subject},
      :predicate {:type :named, :value "http://example.com/name", :term :predicate},
      :object
      {:type :blank,
       :value "_:b5",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}
     {:statement "_:b5 <http://example.com/name> _:b4 .",
      :subject {:type :blank, :value "_:b5", :term :subject},
      :predicate {:type :named, :value "http://example.com/name", :term :predicate},
      :object
      {:type :blank,
       :value "_:b4",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}},
   "_:b3"
   #{{:statement "_:b3 <http://example.com/name> \"graph2\" _:b3 .",
      :subject {:type :blank, :value "_:b3", :term :subject},
      :predicate {:type :named, :value "http://example.com/name", :term :predicate},
      :object
      {:type :literal,
       :term :object,
       :value "graph2",
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :blank, :term :graph, :value "_:b3"}}
     {:statement "_:b1 <http://example.com/friend> _:b0 _:b3 .",
      :subject {:type :blank, :value "_:b1", :term :subject},
      :predicate {:type :named, :value "http://example.com/friend", :term :predicate},
      :object
      {:type :blank,
       :value "_:b0",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :blank, :term :graph, :value "_:b3"}}},
   "_:b1"
   #{{:statement "_:b1 <http://example.com/friend> _:b0 _:b3 .",
      :subject {:type :blank, :value "_:b1", :term :subject},
      :predicate {:type :named, :value "http://example.com/friend", :term :predicate},
      :object
      {:type :blank,
       :value "_:b0",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :blank, :term :graph, :value "_:b3"}}
     {:statement "<http://example.com/1> <http://example.com/friend> _:b1 .",
      :subject {:type :named, :value "http://example.com/1", :term :subject},
      :predicate {:type :named, :value "http://example.com/friend", :term :predicate},
      :object
      {:type :blank,
       :value "_:b1",
       :term :object,
       :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
      :graph {:type :default, :term :graph, :value ""}}}}


  )
