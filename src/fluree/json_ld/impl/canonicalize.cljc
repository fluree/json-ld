(ns fluree.json-ld.impl.canonicalize
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
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
   :issued {}
   :issued-order []})

(defn issue-id
  "Gets the new identifer for the given old identifier, generating it if
  necessary. Returns a tuple of the new issuer state and the issued id [issuer* id]."
  [{:keys [prefix counter issued] :as issuer} bnode]
  (if (issued bnode)
    [issuer bnode]
    (let [id (str prefix counter)
          issuer* (-> issuer
                      (update :issued assoc bnode id)
                      (update :issued-order conj bnode)
                      (update :counter inc))]
      [issuer* id])))

(defn issued-id
  "Returns issued identifier for the given bnode if it has been issued, otherwise nil."
  [issuer bnode]
  (get (:issued issuer) bnode))

(defn relabel-bnode
  "A quirk in the canonicalization spec requires renaming blank nodes during the hash-first-degree-quads
  algorithm, see step 3.1.2."
  [reference-bnode node]
  (if (= reference-bnode (:value node))
    (assoc node :value "_:a")
    (assoc node :value "_:z")))

(defn hash-first-degree-quads
  "Given a bnode identifer `bnode` and the quads `quads` that reference it, return a
  sha256 hash of the normalized quads."
  [quads bnode]
  (let [tranformed-quads (map (fn [quad]
                                (cond-> quad
                                  (= :blank (:type :subject)) (update :subject (partial relabel-bnode bnode))
                                  (= :blank (:type :object)) (update :object (partial relabel-bnode bnode))
                                  (= :blank (:type :graph)) (update :graph (partial relabel-bnode bnode))))
                              quads)]
    (->> (map nquads/->statement tranformed-quads)
         (sort)
         (reduce str)
         (crypto/sha2-256))))

(defn map-bnode-to-quad-info
  "Create a map of bnode ids to the quads that contain them."
  [quads]
  (reduce (fn [bnode->quads {:keys [subject object graph] :as quad}]
            (cond-> bnode->quads
              (= :blank (:type subject)) (update-in [(:value subject) :quads] (fnil conj #{}) quad)
              (= :blank (:type object)) (update-in [(:value object) :quads] (fnil conj #{}) quad)
              (= :blank (:type graph)) (update-in [(:value graph) :quads] (fnil conj #{}) quad)))
          {}
          quads))

(defn initialize-canonicalization-state
  "Creat the initial canonicalization state:

  bnode->quad-info: a map of each bnode identifier to a map of the `:quads` they appear in, as well
  as the `:hash` of those quads.
  ex. {\"_:b0\" {:quads #{...} :hash \"<sha256 of normalized quads>\"}}

  hash->bnodes: a map of each quad hash to the bnodes that reference it.
  ex. {\"<sha256 of normalized quads>\" #{\"_:b0\" \"_:b3\"}}

  canonical-issuer: an issuer state, for tracking the issuance of canonical ids."
  [quads]
  (let [bnode->quad-info (map-bnode-to-quad-info quads)
        bnode->quad-info* (reduce-kv (fn [bnode->quad-info bnode info]
                                       (assoc-in bnode->quad-info [bnode :hash]
                                                 (hash-first-degree-quads (:quads info) bnode)))
                                     bnode->quad-info
                                     bnode->quad-info)
        hash->bnodes (reduce-kv (fn [hash->bnodes bnode info]
                                  (update hash->bnodes
                                          (:hash info)
                                          (fnil conj #{})
                                          bnode))
                                {}
                                bnode->quad-info*)]
    {:canonical-issuer (create-issuer "_:c14n")
     :bnode->quad-info bnode->quad-info*
     :hash->bnodes hash->bnodes}))

(defn hash-related-bnode
  [{:keys [canonical-issuer bnode->quad-info]} related-bnode quad issuer position]
  (let [id (get (:issued canonical-issuer) related-bnode
                (get (:issued issuer) related-bnode
                     (get-in bnode->quad-info [related-bnode :hash])))
        input (str position
                   (when-not (= "g" position) "<" (:value (:predicate quad)) ">")
                   id)]
    (crypto/sha2-256 input)))

(defn map-hash-to-related-bnodes
  [{:keys [bnode->quad-info] :as canon-state} bnode temp-issuer]
  ;; 3) For each quad in quads:

  ;; 3.1) For each component in quad, where component is the subject, object, or graph
  ;; name, and it is a blank node that is not identified by identifier:

  ;; 3.1.1) Set hash to the result of the Hash Related Blank Node algorithm, passing the
  ;; blank node identifier for component as related, quad, path identifier issuer as
  ;; issuer, and position as either s, o, or g based on whether component is a subject,
  ;; object, graph name, respectively.

  ;; 3.1.2) Add a mapping of hash to the blank node identifier for component to hash to
  ;; related blank nodes map, adding an entry as necessary.
  (reduce (fn [hash->related-bnodes quad]
            (reduce (fn [hash->related-bnodes* [term component]]
                      (if (and (= :blank (:type component))
                               (not= bnode (:value component)))
                        (update hash->related-bnodes*
                                (hash-related-bnode canon-state
                                                    (:value component)
                                                    quad
                                                    temp-issuer
                                                    (case term
                                                      :subject "s"
                                                      :object "o"
                                                      :graph "g"))
                                (fnil conj #{})
                                (:value component))
                        hash->related-bnodes*))
                    hash->related-bnodes
                    quad))
          {}
          (:quads (bnode->quad-info bnode))))

(defn- lex-less-than?
  "Given two strings, return `true` if the `a` is lexicographically less than `b`."
  [a b]
  (= a (first (sort [a b]))))

(defn- next-permutation?
  " 5.4.4.3) If chosen path is not empty and the length of path is greater than or equal
  to the length of chosen path and path is lexicographically greater than chosen path,
  then skip to the next permutation.

  Note: Comparing path length to chosen path length can be optimized away; only compare
  lexicographically."
  [chosen-path path]
  (and (pos? (count chosen-path))
       (not (lex-less-than? path chosen-path))))

#_(defn build-hash-input
  [canonical-issuer issuer hash->rel-bnodes ]
  (reduce
    (fn [state0 [rel-hash rel-bnodes]]
      (let [rel-hash* rel-hash]
        (reduce (fn [state1 rel-bnodes-perm]
                  (let [state1* (reduce (fn [state2 rel-bnode]
                                          ;; 5.4.4)
                                          (if-let [canon-bnode-id (issued-id canonical-issuer rel-bnode)]
                                            (update state2 :path str canon-bnode-id)
                                            (if-let [issued-bnode-id (issued-id (:issuer-copy state2) rel-bnode)]
                                              (update state2 :path str issued-bnode-id)
                                              (let [[issuer* id] (issue-id (:issuer-copy state2) rel-bnode)]
                                                (-> state2
                                                    (assoc :issuer-copy issuer*)
                                                    (update :path str id)
                                                    (update :recursion-list conj bnode))))))
                                        state1
                                        rel-bnodes-perm)]
                    (if (next-permutation? (:chosen-path state1*) (:path state1*))
                      state1*

                      (reduce (fn [state2 rel-bnode]
                                (let []))
                              state1
                              (:recursion-list state1*))
                      )

                    ))
                {:path ""
                 :chosen-path ""
                 :recursion-list []
                 :issuer-copy issuer
                 :next-perm false}
                (combo/permutations rel-bnodes))
        (update state0 :data-to-hash str rel-hash*)))
    {:data-to-hash ""
     :chosen-path ""
     :chosen-issuer nil}
    (sort-by first hash->rel-bnodes)))

#_(defn hash-n-degree-quads
  [{:keys [canonical-issuer bnode->quad-info] :as canon-state} bnode temp-issuer]
  (let [hash->rel-bnodes (map-hash-to-related-bnodes canon-state bnode temp-issuer)

        {:keys [data-to-hash]}
        (reduce
          (fn [state0 [rel-hash rel-bnodes]]
            (let [state0* (reduce (fn [state1 rel-bnodes-perm]
                                    (let [state1* (reduce (fn [state2 rel-bnode]
                                                            ;; 5.4.4)
                                                            (if-let [canon-bnode-id (issued-id canonical-issuer bnode)]
                                                              (update state2 :path str canon-bnode-id)
                                                              (if-let [issued-bnode-id (issued-id (:issuer-copy state2) bnode)]
                                                                (update state2 :path str issued-bnode-id)
                                                                (let [[issuer* id] (issue-id (:issuer-copy state2) bnode)]
                                                                  (-> state2
                                                                      (assoc :issuer-copy issuer*)
                                                                      (update :path str id)
                                                                      (update :recursion-list conj bnode))))))
                                                          state1
                                                          rel-bnodes-perm)]
                                      (if (next-permutation? (:chosen-path state1*) (:path state1*))
                                        state1*

                                        (let [state1** (reduce (fn [state2 rel-bnode]
                                                                 ;; 5.4.5
                                                                 (let [result (hash-n-degree-quads canon-state rel-bnode
                                                                                                   (:issuer-copy state1*))
                                                                       [issuer* id] (issue-id (:issuer result) rel-bnode)]
                                                                   (-> state2
                                                                       (update :path str id)
                                                                       (update :path str "<" (:hash result) ">")
                                                                       (assoc :issuer-copy (:issuer result)))))
                                                               state1
                                                               (:recursion-list state1*))]
                                          (if (next-permutation? (:chosen-path state1**) (:path state1**))
                                            state1**
                                            (cond-> state1**
                                              (or (zero? (count (:chosen-path state1**)))
                                                  (lex-less-than? (:path state1**) (:chosen-path state1**)))
                                              (-> state1**
                                                  (assoc :chosen-path (:path state1**))
                                                  (assoc :chosen-issuer (:issuer-copy state1**)))))))))
                                  (-> state0
                                      (assoc :path "")
                                      (assoc :recursion-list [])
                                      (assoc :issuer-copy temp-issuer)
                                      (assoc :next-perm false))
                                  (combo/permutations rel-bnodes))]
              (update state0* :data-to-hash str rel-hash*)))
          {:data-to-hash ""
           :chosen-path ""
           :chosen-issuer nil}
          (sort-by first hash->rel-bnodes))

        data-to-hash (reduce
                       (fn [data-to-hash [rel-hash rel-bnodes]]
                         (let [chosen-path ""
                               chosen-issuer nil
                               permutations
                               (loop [[rel-perm & permutations] (combo/permutations rel-bnodes)
                                      path ""
                                      next-permutation false
                                      perm-issuer temp-issuer
                                      recursion-list []]
                                 (if rel-perm
                                   (let [{path* :path
                                          next-perm* :next-perm
                                          perm-issuer* :perm-issuer
                                          recursion-list* :recursion-list}
                                         (reduce (fn [state rel-bnode]

                                                   (if-let [next-path (get (:issued canonical-issuer) rel-bnode
                                                                           (get (:issued perm-issuer) rel-bnode))]
                                                     (-> state
                                                         (update :path str next-path)
                                                         (assoc :next-perm (next-permutation? chosen-path path)))
                                                     (-> state
                                                         (update :recursion-list conj rel-bnode)
                                                         (assoc :next-perm (next-permutation? chosen-path path)))))
                                                 {:path path
                                                  :perm-issuer perm-issuer
                                                  :recursion-list recursion-list
                                                  :next-perm next-permutation}
                                                 rel-perm)]
                                     (recur permutations path* next-perm* perm-issuer* recursion-list*))
                                   {:recursion-list recursion-list
                                    :perm-issuer perm-issuer
                                    :path path}))]

                           (str data-to-hash rel-hash)))
                       ""
                       (sort-by first hash->rel-bnodes))]))

(defn hash-n-degree-quads
  [{:keys [canonical-issuer bnode->quad-info] :as canon-state} bnode temp-issuer]
  (let [hash->related-bnodes (map-hash-to-related-bnodes canon-state bnode temp-issuer)

        {:keys [data-to-hash chosen-issuer]}
        (reduce
          (fn [state0 [related-hash related-bnodes]]
            (let [state0* (reduce (fn [state1 related-bnodes-permutation]
                                    (let [state1* (reduce (fn [state2 related-bnode]
                                                            ;; 5.4.4)
                                                            (if-let [canon-bnode-id (issued-id canonical-issuer bnode)]
                                                              (update state2 :path str canon-bnode-id)
                                                              (if-let [issued-bnode-id (issued-id (:issuer-copy state2) bnode)]
                                                                (update state2 :path str issued-bnode-id)
                                                                (let [[issuer* id] (issue-id (:issuer-copy state2) bnode)]
                                                                  (-> state2
                                                                      (assoc :issuer-copy issuer*)
                                                                      (update :path str id)
                                                                      (update :recursion-list conj bnode))))))
                                                          state1
                                                          related-bnodes-permutation)]
                                      (if (next-permutation? (:chosen-path state1*) (:path state1*))
                                        state1*

                                        (let [state1** (reduce (fn [state2 related-bnode]
                                                                 ;; 5.4.5
                                                                 (let [{:keys [hash issuer]} (hash-n-degree-quads canon-state related-bnode
                                                                                                                  (:issuer-copy state2))
                                                                       [issuer* id] (issue-id issuer related-bnode)]
                                                                   (-> state2
                                                                       (update :path str id)
                                                                       (update :path str "<" hash ">")
                                                                       (assoc :issuer-copy issuer*))))
                                                               state1*
                                                               (:recursion-list state1*))]
                                          (if (next-permutation? (:chosen-path state1**) (:path state1**))
                                            state1**

                                            (cond-> state1**
                                              ;; 5.4.6
                                              (or (zero? (count (:chosen-path state1**)))
                                                  (lex-less-than? (:path state1**) (:chosen-path state1**)))
                                              (-> (assoc :chosen-path (:path state1**))
                                                  (assoc :chosen-issuer (:issuer-copy state1**)))))))))
                                  (-> state0
                                      (assoc :path "")
                                      (assoc :recursion-list [])
                                      (assoc :issuer-copy temp-issuer)
                                      (assoc :next-perm false))
                                  (combo/permutations related-bnodes))]
              (-> state0*
                  (update :data-to-hash str related-hash (:chosen-path state0*))
                  (assoc :chosen-issuer (:issuer-copy state0*)))))
          {:data-to-hash ""
           :chosen-path ""
           :chosen-issuer nil}
          (sort-by first hash->related-bnodes))

        #_ #_data-to-hash (reduce
                       (fn [data-to-hash [related-hash related-bnodes]]
                         (let [chosen-path ""
                               chosen-issuer nil
                               permutations
                               (loop [[rel-perm & permutations] (combo/permutations related-bnodes)
                                      path ""
                                      next-permutation false
                                      perm-issuer temp-issuer
                                      recursion-list []]
                                 (if rel-perm
                                   (let [{path* :path
                                          next-perm* :next-perm
                                          perm-issuer* :perm-issuer
                                          recursion-list* :recursion-list}
                                         (reduce (fn [state related-bnode]

                                                   (if-let [next-path (get (:issued canonical-issuer) related-bnode
                                                                           (get (:issued perm-issuer) related-bnode))]
                                                     (-> state
                                                         (update :path str next-path)
                                                         (assoc :next-perm (next-permutation? chosen-path path)))
                                                     (-> state
                                                         (update :recursion-list conj related-bnode)
                                                         (assoc :next-perm (next-permutation? chosen-path path)))))
                                                 {:path path
                                                  :perm-issuer perm-issuer
                                                  :recursion-list recursion-list
                                                  :next-perm next-permutation}
                                                 rel-perm)]
                                     (recur permutations path* next-perm* perm-issuer* recursion-list*))
                                   {:recursion-list recursion-list
                                    :perm-issuer perm-issuer
                                    :path path}))]

                           (str data-to-hash related-hash)))
                       ""
                       (sort-by first hash->related-bnodes))]
    {:hash data-to-hash :issuer chosen-issuer}))

(defn assign-canonical-ids
  "Takes the canonicalization state and maps each blank node identifier to a canonical
  blank id identifer, returning the canonical issuer in its final form."
  [{:keys [canonical-issuer hash->bnodes bnode->quad-info] :as canon-state}]
  (let [{:keys [non-uniques canonical-issuer]} ; 5.4
        (->> (keys hash->bnodes)
             (sort)
             (reduce (fn [{:keys [non-uniques canonical-issuer] :as state} hash]
                       (let [bnodes (get hash->bnodes hash)]
                         (if (> (count bnodes) 1)
                           (update state :non-uniques into bnodes)
                           (let [[canonical-issuer*] (issue-id canonical-issuer (first bnodes))]
                             (assoc state :canonical-issuer canonical-issuer*)))))
                     {:canonical-issuer canonical-issuer
                      :non-uniques []}))

        hash-path-list                  ; 6
        (reduce (fn [hash-path-list bnode]
                  (if (issued-id canonical-issuer bnode)
                    hash-path-list
                    (let [temp-issuer (create-issuer "_:b")
                          [temp-issuer*] (issue-id temp-issuer bnode)]
                      (conj hash-path-list
                            (hash-n-degree-quads {:canonical-issuer canonical-issuer
                                                  :hash->bnodes hash->bnodes
                                                  :bnode->quad-info bnode->quad-info}
                                                 bnode
                                                 temp-issuer*)))))
                []
                non-uniques)]
    (->> (sort-by :hash hash-path-list)
         (reduce (fn [canonical-issuer {:keys [issuer]}]
                   (reduce
                     (fn [canonical-issuer bnode]
                       (let [[canonical-issuer*] (issue-id canonical-issuer bnode)]
                         canonical-issuer*))
                     canonical-issuer
                     (:issued-order issuer)))
                 canonical-issuer))))

(defn replace-bnodes
  "Takes a quad and the canonical issuer and replace each quad's blank node identifiers
  with the canonical blank node identifiers."
  [canonical-issuer quad]
  (let [{:keys [subject object graph]} quad]
    (cond-> quad
      (= :blank (:type subject)) (update-in [:subject :value] (partial issued-id canonical-issuer)
                                            #_ #(get (:issued canonical-issuer) %
                                                     (throw (ex-info "No canonical id." {:bnode % :term :subject}))))
      (= :blank (:type object)) (update-in [:object :value] (partial issued-id canonical-issuer)
                                           #_ #(get (:issued canonical-issuer) %
                                                    (throw (ex-info "No canonical id." {:bnode % :term :object}))))
      (= :blank (:type graph)) (update-in [:graph :value] (partial issued-id canonical-issuer)
                                          #_ #(get (:issued canonical-issuer) %
                                                   (throw (ex-info "No canonical id." {:bnode % :term :graph})))))))

(defn canonicalize
  [quads]
  (let [canon-state (initialize-canonicalization-state quads)
        canonical-issuer (assign-canonical-ids canon-state)]
    (->> quads
         (map (partial replace-bnodes canonical-issuer))
         (map nquads/->statement)
         (sort)
         (reduce (fn [doc statement] (str doc statement "\n")) ""))))


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
  (" <http://example.com/name>  ." " <http://example.com/name>  ." "<http://example.com/1> <http://example.com/friend> _:c14n1 ." "<http://example.com/1> <http://example.com/label> \"test\"@en ." "<http://example.com/1> <http://example.com/label> \"test\"^^http://example.com/t1 ." "<http://example.com/2> <http://example.com/count> \"1\" <http://example.com/graphname> ." "<http://example.com/2> <http://example.com/friend> <http://example.com/1> ." "_:c14n0 <http://example.com/name> \"graph2\" _:c14n0 ." "_:c14n1 <http://example.com/friend> _:c14n2 _:c14n0 ." "_:c14n2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  {:prefix "_:c14n", :counter 3, :issued {"_:b3" "_:c14n0", "_:b1" "_:c14n1", "_:b0" "_:c14n2"}, :issued-order ["_:b3" "_:b1" "_:b0"]}
  ({:hash "", :issuer nil} {:hash "", :issuer nil})



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
