(ns fluree.json-ld.impl.canonicalize
  "An implementation of the standard RDF Dataset Canonicalization Algorithm:
  https://json-ld.github.io/rdf-dataset-canonicalization/spec/"
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]
            [lambdaisland.regal :as reg]
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
  (if-let [id (issued bnode)]
    [issuer id]
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
  (let [transformed-quads (map (fn [{:keys [subject object graph] :as quad}]
                                 (cond-> quad
                                   (= :blank (:type subject)) (update :subject (partial relabel-bnode bnode))
                                   (= :blank (:type object)) (update :object (partial relabel-bnode bnode))
                                   (= :blank (:type graph)) (update :graph (partial relabel-bnode bnode))))
                               quads)]
    (->> (map nquads/->statement transformed-quads)
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
  ex. {\"<sha256 of normalized quads>\" [\"_:b0\" \"_:b3\"]}

  canonical-issuer: an issuer state, for tracking the issuance of canonical ids."
  [quads]
  (let [bnode->quad-info (map-bnode-to-quad-info quads)
        bnode->quad-info* (reduce-kv (fn [bnode->quad-info bnode info]
                                       (assoc-in bnode->quad-info [bnode :hash]
                                                 (hash-first-degree-quads (:quads info) bnode)))
                                     bnode->quad-info
                                     bnode->quad-info)
        hash->bnodes (reduce (fn [hash->bnodes [bnode info]]
                                  (update hash->bnodes
                                          (:hash info)
                                          (fnil conj [])
                                          bnode))
                                {}
                                (sort-by first bnode->quad-info*))]
    {:canonical-issuer (create-issuer "_:c14n")
     :bnode->quad-info bnode->quad-info*
     :hash->bnodes hash->bnodes}))

(defn hash-related-bnode
  [{:keys [canonical-issuer bnode->quad-info]} related-bnode quad issuer position]
  ;; 1) Set the identifier to use for related, preferring first the canonical identifier
  ;; for related if issued, second the identifier issued by issuer if issued, and last,
  ;; if necessary, the result of the Hash First Degree Quads algorithm, passing related.
  ;; 2) Initialize a string input to the value of position.
  ;; 3) If position is not g, append <, the value of the predicate in quad, and > to input.
  ;; 4) Append identifier to input.
  ;; 5) Return the hash that results from passing input through the hash algorithm.

  (let [id (or (issued-id canonical-issuer related-bnode)
               (issued-id issuer related-bnode)
               (get-in bnode->quad-info [related-bnode :hash]))
        input (str position
                   (when-not (= "g" position) (str "<" (:value (:predicate quad)) ">"))
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
                      (if (not (and (= :blank (:type component))
                                    (not= bnode (:value component))))
                        hash->related-bnodes*
                        (update hash->related-bnodes*
                                (hash-related-bnode canon-state
                                                    (:value component)
                                                    quad
                                                    temp-issuer
                                                    (case term :subject "s" :object "o" :graph "g"))
                                (fnil conj #{})
                                (:value component))))
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
  [{:keys [chosen-path path]}]
  (and (pos? (count chosen-path))
       (not (lex-less-than? path chosen-path))))

(defn process-permutation-bnode1
  [{:keys [canonical-issuer] :as canon-state} {:keys [issuer-copy] :as hndq-state} related-bnode]
  ;; 5.4.4)
  (println (:recurse-level hndq-state) "bnode1" related-bnode (pr-str hndq-state))
  (if-let [canon-bnode-id (issued-id canonical-issuer related-bnode)]
    (update hndq-state :path str canon-bnode-id)
    (if-let [issued-bnode-id (issued-id issuer-copy related-bnode)]
      (update hndq-state :path str issued-bnode-id)
      (let [[issuer* id] (issue-id issuer-copy related-bnode)]
        (-> hndq-state
            (assoc :issuer-copy issuer*)
            (update :path str id)
            (update :recursion-list conj related-bnode))))))

(declare hash-n-degree-quads)
(defn process-permutation-bnode2
  [canon-state {:keys [issuer-copy] :as hndq-state} related-bnode]
  ;; 5.4.5
  (println (:recurse-level hndq-state) "bnode2" related-bnode (pr-str hndq-state))
  (let [{:keys [hash issuer]} (hash-n-degree-quads canon-state related-bnode issuer-copy (str (:recurse-level hndq-state) "-"))
        [issuer* id] (issue-id issuer related-bnode)]
    (-> hndq-state
        (update :path str id)
        (update :path str "<" hash ">")
        (assoc :issuer-copy issuer*))))

(defn process-permutation
  [canon-state issuer hndq-state related-bnodes-permutation]
  (println (:recurse-level hndq-state) "permutation-0" related-bnodes-permutation issuer (pr-str hndq-state))
  (let [hndq-state (reduce (partial process-permutation-bnode1 canon-state)
                           (-> hndq-state
                               (assoc :path "")
                               (assoc :recursion-list [])
                               (assoc :next-perm false)
                               ;; ???
                               (assoc :issuer-copy (or (:issuer-copy hndq-state) issuer)))
                           related-bnodes-permutation)]
    (println (:recurse-level hndq-state) "permutation-1" (pr-str hndq-state))
    (if (next-permutation? hndq-state)
      hndq-state
      (let [hndq-state (reduce (partial process-permutation-bnode2 canon-state)
                               hndq-state
                               (:recursion-list hndq-state))]
        (println (:recurse-level hndq-state) "permutation-2" (pr-str hndq-state))
        (if (next-permutation? hndq-state)
          hndq-state
          (let [hndq-state (cond-> hndq-state
                             ;; 5.4.6
                             (or (zero? (count (:chosen-path hndq-state)))
                                 (lex-less-than? (:path hndq-state) (:chosen-path hndq-state)))
                             (-> (assoc :chosen-path (:path hndq-state))
                                 (assoc :chosen-issuer (:issuer-copy hndq-state))))]
            (println (:recurse-level hndq-state) "permutation-3" (pr-str hndq-state))
            hndq-state))))))

(defn process-related-bnodes
  [canon-state issuer hndq-state [related-hash related-bnodes]]
  (println (:recurse-level hndq-state) "related-bnodes-start" related-hash related-bnodes (pr-str hndq-state))
  (let [hndq-state (reduce (partial process-permutation canon-state issuer)
                           (-> hndq-state
                               (assoc :chosen-path "")
                               (assoc :chosen-issuer nil)
                               (update :data-to-hash str related-hash))
                           (combo/permutations related-bnodes))]
    (println (:recurse-level hndq-state) "related-bnodes-end" (pr-str hndq-state))
    (update hndq-state :data-to-hash str (:chosen-path hndq-state))))

(defn hash-n-degree-quads
  [{:keys [canonical-issuer bnode->quad-info] :as canon-state} bnode issuer & [recurse-level]]
  (let [recurse-level (str recurse-level)
        hash->related-bnodes (map-hash-to-related-bnodes canon-state bnode issuer)
        _ (println recurse-level "hndq" (pr-str bnode) (pr-str issuer))
        _ (println recurse-level "hash->related-bnodes" (pr-str hash->related-bnodes))
        {:keys [data-to-hash chosen-issuer]}
        (reduce (partial process-related-bnodes canon-state issuer)
                {:recurse-level recurse-level}
                (sort-by first hash->related-bnodes))]
    (println recurse-level "RESULT" data-to-hash (:issued chosen-issuer) (:issued issuer))
    {:hash (crypto/sha2-256 data-to-hash) :issuer (or chosen-issuer issuer)}))

(defn assign-canonical-ids
  "Takes the canonicalization state and maps each blank node identifier to a canonical
  blank id identifer, returning the canonical issuer in its final form."
  [{:keys [canonical-issuer hash->bnodes bnode->quad-info] :as canon-state}]
  (let [{:keys [non-uniques canonical-issuer]} ; 5.4
        (->> (sort-by first hash->bnodes)
             (reduce (fn [{:keys [non-uniques canonical-issuer] :as state} [_hash bnodes]]
                       (if (> (count bnodes) 1)
                         (update state :non-uniques conj bnodes)
                         (let [[canonical-issuer*] (issue-id canonical-issuer (first bnodes))]
                           (assoc state :canonical-issuer canonical-issuer*))))
                     {:canonical-issuer canonical-issuer
                      :non-uniques []}))
        canonical-issuer                ; 6
        (reduce (fn [canonical-issuer bnodes]
                  (let [hash-path-list
                        (reduce (fn [hash-path-list bnode]
                                  (println "id" bnode)
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
                                bnodes)]
                    (->> (sort-by :hash hash-path-list)
                         (reduce (fn [canonical-issuer {:keys [issuer]}]
                                   (reduce
                                     (fn [canonical-issuer bnode]
                                       (let [[canonical-issuer*] (issue-id canonical-issuer bnode)]
                                         canonical-issuer*))
                                     canonical-issuer
                                     (:issued-order issuer)))
                                 canonical-issuer))))
                canonical-issuer
                non-uniques)]
    canonical-issuer))

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
         (reduce str))))

(comment
  zzz
  #{{:statement "_:b4 <http://example.org/vocab#p> _:b1 .",
     :subject {:type :blank, :value "_:b4", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b1", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b0 <http://example.org/vocab#p> _:b1 .",
     :subject {:type :blank, :value "_:b0", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b1", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b1 <http://example.org/vocab#p> _:b3 .",
     :subject {:type :blank, :value "_:b1", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b3", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b10 <http://example.org/vocab#p> _:b9 .",
     :subject {:type :blank, :value "_:b10", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b9", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b7 <http://example.org/vocab#p> _:b6 .",
     :subject {:type :blank, :value "_:b7", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b6", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b5 <http://example.org/vocab#p> _:b2 .",
     :subject {:type :blank, :value "_:b5", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b2", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b10 <http://example.org/vocab#p> _:b8 .",
     :subject {:type :blank, :value "_:b10", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b8", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b10 <http://example.org/vocab#p> _:b7 .",
     :subject {:type :blank, :value "_:b10", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b7", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b7 <http://example.org/vocab#p> _:b10 .",
     :subject {:type :blank, :value "_:b7", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b10", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b3 <http://example.org/vocab#p> _:b1 .",
     :subject {:type :blank, :value "_:b3", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b1", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b3 <http://example.org/vocab#p> _:b5 .",
     :subject {:type :blank, :value "_:b3", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b5", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b9 <http://example.org/vocab#p> _:b10 .",
     :subject {:type :blank, :value "_:b9", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b10", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b3 <http://example.org/vocab#p> _:b0 .",
     :subject {:type :blank, :value "_:b3", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b0", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b4 <http://example.org/vocab#p> _:b2 .",
     :subject {:type :blank, :value "_:b4", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b2", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b6 <http://example.org/vocab#p> _:b7 .",
     :subject {:type :blank, :value "_:b6", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b7", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b11 <http://example.org/vocab#p> _:b8 .",
     :subject {:type :blank, :value "_:b11", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b8", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b8 <http://example.org/vocab#p> _:b11 .",
     :subject {:type :blank, :value "_:b8", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b11", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b5 <http://example.org/vocab#p> _:b4 .",
     :subject {:type :blank, :value "_:b5", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b4", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b1 <http://example.org/vocab#p> _:b0 .",
     :subject {:type :blank, :value "_:b1", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b0", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b2 <http://example.org/vocab#p> _:b0 .",
     :subject {:type :blank, :value "_:b2", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b0", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b9 <http://example.org/vocab#p> _:b6 .",
     :subject {:type :blank, :value "_:b9", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b6", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b11 <http://example.org/vocab#p> _:b9 .",
     :subject {:type :blank, :value "_:b11", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b9", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b5 <http://example.org/vocab#p> _:b3 .",
     :subject {:type :blank, :value "_:b5", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b3", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b6 <http://example.org/vocab#p> _:b8 .",
     :subject {:type :blank, :value "_:b6", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b8", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b8 <http://example.org/vocab#p> _:b6 .",
     :subject {:type :blank, :value "_:b8", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b6", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b11 <http://example.org/vocab#p> _:b7 .",
     :subject {:type :blank, :value "_:b11", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b7", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b2 <http://example.org/vocab#p> _:b4 .",
     :subject {:type :blank, :value "_:b2", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b4", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b0 <http://example.org/vocab#p> _:b3 .",
     :subject {:type :blank, :value "_:b0", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b3", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b7 <http://example.org/vocab#p> _:b11 .",
     :subject {:type :blank, :value "_:b7", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b11", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b2 <http://example.org/vocab#p> _:b5 .",
     :subject {:type :blank, :value "_:b2", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b5", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b8 <http://example.org/vocab#p> _:b10 .",
     :subject {:type :blank, :value "_:b8", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b10", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b0 <http://example.org/vocab#p> _:b2 .",
     :subject {:type :blank, :value "_:b0", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b2", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b6 <http://example.org/vocab#p> _:b9 .",
     :subject {:type :blank, :value "_:b6", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b9", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b4 <http://example.org/vocab#p> _:b5 .",
     :subject {:type :blank, :value "_:b4", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b5", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b9 <http://example.org/vocab#p> _:b11 .",
     :subject {:type :blank, :value "_:b9", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b11", :term :object},
     :graph {:type :default, :term :graph, :value ""}}
    {:statement "_:b1 <http://example.org/vocab#p> _:b4 .",
     :subject {:type :blank, :value "_:b1", :term :subject},
     :predicate {:type :named, :value "http://example.org/vocab#p", :term :predicate},
     :object {:type :blank, :value "_:b4", :term :object},
     :graph {:type :default, :term :graph, :value ""}}}


  (mapv nquads/->statement zzz)
  ["_:b5 <http://example.org/vocab#p> _:b2 .\n"
   "_:b8 <http://example.org/vocab#p> _:b10 .\n"
   "_:b4 <http://example.org/vocab#p> _:b2 .\n"
   "_:b9 <http://example.org/vocab#p> _:b10 .\n"
   "_:b0 <http://example.org/vocab#p> _:b3 .\n"
   "_:b0 <http://example.org/vocab#p> _:b1 .\n"
   "_:b0 <http://example.org/vocab#p> _:b2 .\n"
   "_:b10 <http://example.org/vocab#p> _:b7 .\n"
   "_:b9 <http://example.org/vocab#p> _:b6 .\n"
   "_:b6 <http://example.org/vocab#p> _:b9 .\n"
   "_:b6 <http://example.org/vocab#p> _:b7 .\n"
   "_:b7 <http://example.org/vocab#p> _:b6 .\n"
   "_:b11 <http://example.org/vocab#p> _:b9 .\n"
   "_:b4 <http://example.org/vocab#p> _:b5 .\n"
   "_:b1 <http://example.org/vocab#p> _:b3 .\n"
   "_:b1 <http://example.org/vocab#p> _:b4 .\n"
   "_:b6 <http://example.org/vocab#p> _:b8 .\n"
   "_:b9 <http://example.org/vocab#p> _:b11 .\n"
   "_:b4 <http://example.org/vocab#p> _:b1 .\n"
   "_:b1 <http://example.org/vocab#p> _:b0 .\n"
   "_:b11 <http://example.org/vocab#p> _:b8 .\n"
   "_:b3 <http://example.org/vocab#p> _:b1 .\n"
   "_:b8 <http://example.org/vocab#p> _:b11 .\n"
   "_:b5 <http://example.org/vocab#p> _:b4 .\n"
   "_:b8 <http://example.org/vocab#p> _:b6 .\n"
   "_:b2 <http://example.org/vocab#p> _:b4 .\n"
   "_:b3 <http://example.org/vocab#p> _:b0 .\n"
   "_:b5 <http://example.org/vocab#p> _:b3 .\n"
   "_:b11 <http://example.org/vocab#p> _:b7 .\n"
   "_:b2 <http://example.org/vocab#p> _:b0 .\n"
   "_:b2 <http://example.org/vocab#p> _:b5 .\n"
   "_:b10 <http://example.org/vocab#p> _:b8 .\n"
   "_:b10 <http://example.org/vocab#p> _:b9 .\n"
   "_:b7 <http://example.org/vocab#p> _:b11 .\n"
   "_:b7 <http://example.org/vocab#p> _:b10 .\n"
   "_:b3 <http://example.org/vocab#p> _:b5 .\n"]

  (def yyy (initialize-canonicalization-state zzz))
  yyy
  {:canonical-issuer {:prefix "_:c14n", :counter 0, :issued {}, :issued-order []},
   :bnode->quad-info
   {"_:b11"
    {:quads
     #{{:statement "_:b11 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b9 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b11 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b8 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b11 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b10"
    {:quads
     #{{:statement "_:b8 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b9 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b10 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b10 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b10 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b5"
    {:quads
     #{{:statement "_:b5 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b4 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b5 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b5 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b8"
    {:quads
     #{{:statement "_:b8 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b11 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b8 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b8 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b10 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b9"
    {:quads
     #{{:statement "_:b9 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b9 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b11 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b9 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b10 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b1"
    {:quads
     #{{:statement "_:b0 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b4 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b0"
    {:quads
     #{{:statement "_:b0 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b0 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b0 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b6"
    {:quads
     #{{:statement "_:b9 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b9", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b9 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b9",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b8 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b8",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b8 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b8", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b2"
    {:quads
     #{{:statement "_:b5 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b4 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b0 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b7"
    {:quads
     #{{:statement "_:b10 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b10", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b6 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b6", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b6 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b6",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b11 <http://example.org/vocab#p> _:b7 .",
        :subject {:type :blank, :value "_:b11", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b7",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b11 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b11",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b7 <http://example.org/vocab#p> _:b10 .",
        :subject {:type :blank, :value "_:b7", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b10",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b4"
    {:quads
     #{{:statement "_:b4 <http://example.org/vocab#p> _:b2 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b2",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b4 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b4 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b4", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b5 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b2 <http://example.org/vocab#p> _:b4 .",
        :subject {:type :blank, :value "_:b2", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b4",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"},
    "_:b3"
    {:quads
     #{{:statement "_:b0 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b0", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b1 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b1", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b1 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b1",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b0 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b0",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b5 <http://example.org/vocab#p> _:b3 .",
        :subject {:type :blank, :value "_:b5", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b3",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}
       {:statement "_:b3 <http://example.org/vocab#p> _:b5 .",
        :subject {:type :blank, :value "_:b3", :term :subject},
        :predicate
        {:type :named, :value "http://example.org/vocab#p", :term :predicate},
        :object
        {:type :blank,
         :value "_:b5",
         :term :object,
         :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
        :graph {:type :default, :term :graph, :value ""}}},
     :hash "7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"}},
   :hash->bnodes
   {"7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"
    ["_:b0"
     "_:b1"
     "_:b10"
     "_:b11"
     "_:b2"
     "_:b3"
     "_:b4"
     "_:b5"
     "_:b6"
     "_:b7"
     "_:b8"
     "_:b9"]}}

  ;; non-normalized
  ;; ['_:b0','_:b1','_:b2','_:b3','_:b4','_:b5','_:b6','_:b7','_:b8','_:b9','_:b10','_:b11']

  ["_:b0","_:b1","_:b2","_:b3","_:b4","_:b5","_:b6","_:b7","_:b8","_:b9","_:b10","_:b11"]

  (:hash->bnodes (initialize-canonicalization-state zzz))
  {"7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"
   ["_:b0" "_:b1" "_:b10" "_:b11" "_:b2" "_:b3" "_:b4" "_:b5" "_:b6" "_:b7" "_:b8" "_:b9"]}
  {"7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"
   ["_:b0" "_:b1" "_:b2" "_:b3" "_:b4" "_:b5" "_:b6" "_:b7" "_:b8" "_:b9" "_:b10" "_:b11"]}

  (sort '("_:b11" "_:b10" "_:b5" "_:b8" "_:b9" "_:b1" "_:b0" "_:b6" "_:b2" "_:b7" "_:b4" "_:b3"))

  (defn to-tuple [s]
    (let [[_ s 'd] (re-matches #"(a)(\d+)" s)]
      [s '(Long/parseLong d)]))

  [:cat [:capture "_:" ]]

  ("_:b0" "_:b1" "_:b10" "_:b11" "_:b2" "_:b3" "_:b4" "_:b5" "_:b6" "_:b7" "_:b8" "_:b9")

  (sort (keys (:bnode->quad-info yyy)))
  '("_:b0" "_:b1" "_:b10" "_:b11" "_:b2" "_:b3" "_:b4" "_:b5" "_:b6" "_:b7" "_:b8" "_:b9")

  '("_:b11" "_:b10" "_:b5" "_:b8" "_:b9" "_:b1" "_:b0" "_:b6" "_:b2" "_:b7" "_:b4" "_:b3")

  "a0" "a1" "a2" "a3" "a4" "a9" "a10" "a11"
  "a0" "a1" "a10" "a11" "a2" "a3" "a4" "a9"


  (:canonical-issuer :bnode->quad-info :hash->bnodes)

  ;; if we're sorting by hash, and they all have the same hash, what's the next order?
  {"7215844358b318c724bd2de6ce59b01e6b8e2a4a24c8da82b77862448f8263fd"
   ["_:b0"
    "_:b1"
    "_:b2"
    "_:b3"
    "_:b4"
    "_:b5"
    "_:b6"
    "_:b7"
    "_:b8"
    "_:b9"
    "_:b10"
    "_:b11"]}


  (assign-canonical-ids yyy)
  {:prefix "_:c14n",
   :counter 12,
   :issued
   {"_:b11" "_:c14n10",
    "_:b10" "_:c14n11",
    "_:b5" "_:c14n0",
    "_:b8" "_:c14n7",
    "_:b9" "_:c14n8",
    "_:b1" "_:c14n5",
    "_:b0" "_:c14n4",
    "_:b6" "_:c14n6",
    "_:b2" "_:c14n1",
    "_:b7" "_:c14n9",
    "_:b4" "_:c14n2",
    "_:b3" "_:c14n3"},
   :issued-order
   ["_:b5"
    "_:b2"
    "_:b4"
    "_:b3"
    "_:b0"
    "_:b1"
    "_:b6"
    "_:b8"
    "_:b9"
    "_:b7"
    "_:b11"
    "_:b10"]}

  {:prefix "_:c14n",
   :counter 12,
   :issued
   {"_:b11" "_:c14n10",
    "_:b10" "_:c14n6",
    "_:b5" "_:c14n0",
    "_:b8" "_:c14n7",
    "_:b9" "_:c14n8",
    "_:b1" "_:c14n5",
    "_:b0" "_:c14n4",
    "_:b6" "_:c14n11",
    "_:b2" "_:c14n1",
    "_:b7" "_:c14n9",
    "_:b4" "_:c14n2",
    "_:b3" "_:c14n3"},
   :issued-order
   ["_:b5"
    "_:b2"
    "_:b4"
    "_:b3"
    "_:b0"
    "_:b1"
    "_:b10"
    "_:b8"
    "_:b9"
    "_:b7"
    "_:b11"
    "_:b6"]}

  {
   "_:b0" "_:c14n0",
   "_:b2" "_:c14n1",
   "_:b3" "_:c14n2",
   "_:b1" "_:c14n3",
   "_:b4" "_:c14n4",
   "_:b5" "_:c14n5",
   "_:b6" "_:c14n6",
   "_:b7" "_:c14n7",
   "_:b8" "_:c14n8",
   "_:b9" "_:c14n9",
   "_:b10" "_:c14n10",
   "_:b11" "_:c14n11"}


  ,)
