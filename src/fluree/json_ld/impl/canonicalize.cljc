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
  ;; 4.5
  [{:keys [prefix counter issued] :as issuer} bnode]
  (if-let [id (issued bnode)]           ; 1
    [issuer id]
    (let [id (str prefix counter)       ; 2
          issuer* (-> issuer
                      (update :issued assoc bnode id) ; 3
                      (update :issued-order conj bnode) ; need to keep track of issued order (clj maps aren't insertion-sorted)
                      (update :counter inc))]           ; 4
      [issuer* id])))                                   ; 5

(defn issued-id
  "Returns issued identifier for the given bnode if it has been issued, otherwise nil."
  [issuer bnode]
  (get (:issued issuer) bnode))

(defn relabel-bnode
  "A quirk in the canonicalization spec requires renaming blank nodes during the hash-first-degree-quads
  algorithm, see step 3.1.1.1."
  [reference-bnode node]
  (if (= reference-bnode (:value node)) ; 3.1.1.1
    (assoc node :value "_:a")
    (assoc node :value "_:z")))

(defn hash-first-degree-quads
  "Given a bnode identifer `bnode` and the quads `quads` that reference it, return a
  sha256 hash of the normalized quads."
  [quads bnode]                         ; 2
  ;; 4.6
  ;; 1. Initialize nquads to an empty list. It will be used to store quads in N-Quads format.
  ;; 2. Get the list of quads quads associated with the reference blank node identifier in the blank node to quads map.
  ;; 3. For each quad quad in quads:
  ;; 3.1 Serialize the quad in N-Quads format with the following special rule:
  ;; 3.1.1 If any component in quad is an blank node, then serialize it using a special identifier as follows:
  ;; 3.1.1.1 If the blank node's existing blank node identifier matches the reference blank node identifier
  ;; then use the blank node identifier _:a, otherwise, use the blank node identifier _:z.
  ;; 4. Sort nquads in lexicographical order.
  ;; 5. Return the hash that results from passing the sorted, joined nquads through the hash algorithm.
  (let [transformed-quads (map (fn [{:keys [subject object graph] :as quad}] ; 3
                                 (cond-> quad                                ; 3.1.1
                                   (= :blank (:type subject)) (update :subject (partial relabel-bnode bnode))
                                   (= :blank (:type object)) (update :object (partial relabel-bnode bnode))
                                   (= :blank (:type graph)) (update :graph (partial relabel-bnode bnode))))
                               quads)]
    (->> (nquads/serialize transformed-quads) ; 3.1, 4
         (crypto/sha2-256))))                 ; 5

(defn map-bnode-to-quad-info
  "Create a map of bnode ids to the quads that contain them."
  [quads]
  ;; 2.1 For each blank node that occurs in the quad, add a reference to the quad using
  ;; the blank node identifier in the blank node to quads map, creating a new entry if
  ;; necessary.
  ;; Normalization of quads happens in nquads/parse, semantically equivalent quads are merged
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
  (let [bnode->quad-info (map-bnode-to-quad-info quads)                ; 2
        bnode->quad-info* (reduce-kv (fn [bnode->quad-info bnode info] ; 3, 4 elided, 5
                                       ;; 5.3
                                       (assoc-in bnode->quad-info [bnode :hash]
                                                 (hash-first-degree-quads (:quads info) bnode))) ; 5.3.1
                                     bnode->quad-info
                                     bnode->quad-info)

        hash->bnodes (reduce (fn [hash->bnodes [bnode info]]
                               (update hash->bnodes
                                       (:hash info) ; 5.3.2
                                       (fnil conj [])
                                       bnode))
                             {}
                             (sort-by first bnode->quad-info*))
        canon-issuer (create-issuer "_:c14n")]
    {:canonical-issuer canon-issuer
     :bnode->quad-info bnode->quad-info*
     :hash->bnodes hash->bnodes}))

(defn hash-related-bnode
  [{:keys [canonical-issuer bnode->quad-info]} related-bnode quad issuer position]
  ;; 4.7
  ;; 1) Set the identifier to use for related, preferring first the canonical identifier
  ;; for related if issued, second the identifier issued by issuer if issued, and last,
  ;; if necessary, the result of the Hash First Degree Quads algorithm, passing related.
  ;; 2) Initialize a string input to the value of position.
  ;; 3) If position is not g, append <, the value of the predicate in quad, and > to input.
  ;; 4) Append identifier to input.
  ;; 5) Return the hash that results from passing input through the hash algorithm.

  (let [id (or (issued-id canonical-issuer related-bnode) ; 1
               (issued-id issuer related-bnode)
               (get-in bnode->quad-info [related-bnode :hash]))
        input (str position             ; 2
                   (when-not (= "g" position) (str "<" (:value (:predicate quad)) ">")) ; 3
                   id)]                 ; 4
    (crypto/sha2-256 input)))           ; 5

(defn map-hash-to-related-bnodes
  [{:keys [bnode->quad-info] :as canon-state} bnode issuer]
  ;; 3) For each quad in quads:

  ;; 3.1) For each component in quad, where component is the subject, object, or graph
  ;; name, and it is a blank node that is not identified by identifier:

  ;; 3.1.1) Set hash to the result of the Hash Related Blank Node algorithm, passing the
  ;; blank node identifier for component as related, quad, path identifier issuer as
  ;; issuer, and position as either s, o, or g based on whether component is a subject,
  ;; object, graph name, respectively.

  ;; 3.1.2) Add a mapping of hash to the blank node identifier for component to hash to
  ;; related blank nodes map, adding an entry as necessary.
  (reduce (fn [hash->related-bnodes quad] ; 3
            (reduce (fn [hash->related-bnodes* [term component]]
                      (if (and (= :blank (:type component)) ; 3.1
                               (not= bnode (:value component)))
                        (update hash->related-bnodes*
                                ;; 3.1.1
                                (hash-related-bnode canon-state
                                                    (:value component)
                                                    quad
                                                    issuer
                                                    (case term :subject "s" :object "o" :graph "g"))
                                (fnil conj #{}) ; 3.1.2
                                (:value component))
                        hash->related-bnodes*))
                    hash->related-bnodes
                    quad))
          {}                                  ; 1
          (:quads (bnode->quad-info bnode)))) ; 2

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
  (if-let [canon-bnode-id (issued-id canonical-issuer related-bnode)]
    (update hndq-state :path str canon-bnode-id) ; 5.4.4.1
    (if-let [issued-bnode-id (issued-id issuer-copy related-bnode)]
      (update hndq-state :path str issued-bnode-id)
      (let [[issuer* id] (issue-id issuer-copy related-bnode)] ; 5.4.4.2.2
        (-> hndq-state
            (update :recursion-list conj related-bnode) ; 5.4.4.2.1
            (assoc :issuer-copy issuer*)
            (update :path str id))))))  ; 5.4.4.2.2

(declare hash-n-degree-quads)
(defn process-permutation-bnode2
  [canon-state {:keys [issuer-copy] :as hndq-state} related-bnode]
  ;; 5.4.5
  (let [{:keys [hash issuer]} (hash-n-degree-quads canon-state related-bnode issuer-copy (str (:recurse-level hndq-state) "-")) ; 5.4.5.1
        [issuer-copy* id] (issue-id issuer-copy related-bnode)] ; 5.4.5.2
    (-> hndq-state
        (update :path str id)           ; 5.4.5.2
        (update :path str "<" hash ">") ; 5.4.5.3
        (assoc :issuer-copy issuer)))) ; 5.4.5.4

(defn process-permutation
  [canon-state hndq-state related-bnodes-permutation]
  (let [issuer-copy (:issuer hndq-state)
        hndq-state (reduce (partial process-permutation-bnode1 canon-state) ; 5.4.4-5.4.4.2.2
                           (-> hndq-state
                               (assoc :issuer-copy issuer-copy) ; 5.4.1
                               (assoc :path "")                 ; 5.4.2
                               (assoc :recursion-list [])       ; 5.4.3
                               (assoc :next-perm false))
                           related-bnodes-permutation)]
    (if (and (pos? (count (:chosen-path hndq-state)))
             (not (lex-less-than? (:path hndq-state) (:chosen-path hndq-state)))) ; 5.4.4.3
      hndq-state
      (let [hndq-state (reduce (partial process-permutation-bnode2 canon-state) ; 5.4.5.1-5.4.5.5
                               hndq-state
                               (:recursion-list hndq-state)) ; 5.4.5
            hndq-state* (cond-> hndq-state
                         ;; 5.4.6
                         (or (zero? (count (:chosen-path hndq-state)))
                             (lex-less-than? (:path hndq-state) (:chosen-path hndq-state)))
                         (-> (assoc :chosen-path (:path hndq-state))
                             (assoc :chosen-issuer (:issuer-copy hndq-state))))]
        hndq-state*))))

(defn process-related-bnodes
  [canon-state hndq-state [related-hash related-bnodes]]
  (let [permutations (combo/permutations (sort related-bnodes))
        hndq-state (reduce (partial process-permutation canon-state) ; 5.4.1-5.4.6
                           (-> hndq-state
                               (update :data-to-hash str related-hash) ; 5.1
                               (assoc :chosen-path "")                 ; 5.2
                               (assoc :chosen-issuer nil))             ; 5.3
                           permutations)]       ; 5.4
    (-> hndq-state
        (update :data-to-hash str (:chosen-path hndq-state))
        (assoc :issuer (:chosen-issuer hndq-state)))))

(defn hash-n-degree-quads
  [{:keys [canonical-issuer bnode->quad-info] :as canon-state} bnode issuer & [recurse-level]]
  ;; 4.8
  (let [recurse-level (str recurse-level)
        hash->related-bnodes (map-hash-to-related-bnodes canon-state bnode issuer) ; 1-3.1.2
        {:keys [data-to-hash issuer]}
        (reduce (partial process-related-bnodes canon-state) ; 4-
                {:recurse-level recurse-level
                 :issuer issuer}
                (sort-by first hash->related-bnodes))] ; 5.
    {:hash (crypto/sha2-256 data-to-hash) :issuer issuer})) ; 5.6, 6

(defn assign-canonical-ids
  "Takes the canonicalization state and maps each blank node identifier to a canonical
  blank id identifer, returning the canonical issuer in its final form."
  [{:keys [canonical-issuer hash->bnodes bnode->quad-info] :as canon-state}]
  (let [{:keys [non-uniques canonical-issuer]} ; 5.4
        (->> (sort-by first hash->bnodes)
             (reduce (fn [{:keys [non-uniques canonical-issuer] :as state} [_hash bnodes]]
                       (if (> (count bnodes) 1)
                         (update state :non-uniques conj bnodes) ; 5.4.1
                         (let [[canonical-issuer*] (issue-id canonical-issuer (first bnodes))] ; 5.4.2
                           (assoc state :canonical-issuer canonical-issuer*)))) ; 5.4.3-5 elided
                     {:canonical-issuer canonical-issuer
                      :non-uniques []}))
        canonical-issuer
        (reduce (fn [canonical-issuer bnodes] ; 6
                  (let [hash-path-list  ; 6.1 - [{:hash "..." :issuer {...}}]
                        (reduce (fn [hash-path-list bnode] ; 6.2
                                  (if (issued-id canonical-issuer bnode)
                                    hash-path-list ; 6.2.1
                                    (let [issuer (create-issuer "_:b") ; 6.2.2
                                          [issuer*] (issue-id issuer bnode)] ; 6.2.3
                                      (conj hash-path-list
                                            (hash-n-degree-quads {:canonical-issuer canonical-issuer ; 6.2.4
                                                                  :hash->bnodes hash->bnodes
                                                                  :bnode->quad-info bnode->quad-info}
                                                                 bnode
                                                                 issuer*)))))
                                []
                                bnodes)]
                    (->> (sort-by :hash hash-path-list) ; 6.3
                         (reduce (fn [canonical-issuer {:keys [issuer]}]
                                   (reduce (fn [canonical-issuer bnode]
                                             (let [[canonical-issuer*] (issue-id canonical-issuer bnode)] ; 6.3.1
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
      (= :blank (:type subject)) (update-in [:subject :value] (partial issued-id canonical-issuer))
      (= :blank (:type object)) (update-in [:object :value] (partial issued-id canonical-issuer))
      (= :blank (:type graph)) (update-in [:graph :value] (partial issued-id canonical-issuer)))))

;; 4.4 Canonicalization Algorithm
(defn canonicalize
  [quads]
  (let [canon-state (initialize-canonicalization-state quads) ; 1 - 5.3.2
        canonical-issuer (assign-canonical-ids canon-state)]  ; 5.4 - 6.3.1
    (->> quads                                           ; 7
         (map (partial replace-bnodes canonical-issuer)) ; 7.1, 7.2
         nquads/serialize)))                                  ; 8
