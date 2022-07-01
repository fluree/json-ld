(ns fluree.json-ld.impl.canonicalize
  "An implementation of the standard RDF Dataset Canonicalization Algorithm:
  https://json-ld.github.io/rdf-dataset-canonicalization/spec/"
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]
            [lambdaisland.regal :as reg]
            [fluree.json-ld.impl.nquads :as nquads]
            [fluree.crypto :as crypto]))

(defonce issuer-id (atom -1))
(comment
  (reset! issuer-id -1)
  @issuer-id
  ,)
(defn create-issuer
  [prefix]
  {:id (swap! issuer-id inc)
   :prefix prefix
   :counter 0
   :issued {}
   :issued-order []})

(defn clone-issuer
  [issuer]
  (assoc issuer :id (swap! issuer-id inc)))

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
    (println "create canon issuer", (:id canon-issuer))
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
  (reduce (fn [hash->related-bnodes quad] ; 3
            (reduce (fn [hash->related-bnodes* [term component]]
                      (if (and (= :blank (:type component)) ; 3.1
                               (not= bnode (:value component)))
                        (update hash->related-bnodes*
                                ;; 3.1.1
                                (hash-related-bnode canon-state
                                                    (:value component)
                                                    quad
                                                    temp-issuer
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
  #_(println (:recurse-level hndq-state) "bnode1" related-bnode (pr-str hndq-state))
  (if-let [canon-bnode-id (issued-id canonical-issuer related-bnode)]
    (update hndq-state :path str canon-bnode-id) ; 5.4.4.1
    (if-let [issued-bnode-id (issued-id issuer-copy related-bnode)]
      (update hndq-state :path str issued-bnode-id)
      (let [[issuer* id] (issue-id issuer-copy related-bnode)] ; 5.4.4.2.2
        (println (:recurse-level hndq-state) "issue issuerCopy id 0 by" (:id issuer-copy) "from" related-bnode "path" (str (:path hndq-state) id))
        (-> hndq-state
            (update :recursion-list conj related-bnode) ; 5.4.4.2.1
            (assoc :issuer-copy issuer*)
            (update :path str id))))))  ; 5.4.4.2.2

(declare hash-n-degree-quads)
(defn process-permutation-bnode2
  [canon-state {:keys [issuer-copy] :as hndq-state} related-bnode]
  ;; 5.4.5
  #_(println (:recurse-level hndq-state) "bnode2" related-bnode (pr-str hndq-state))
  (let [{:keys [hash issuer]} (hash-n-degree-quads canon-state related-bnode issuer-copy (str (:recurse-level hndq-state) "-")) ; 5.4.5.1
        [issuer-copy* id] (issue-id issuer-copy related-bnode)] ; 5.4.5.2
    (println (:recurse-level hndq-state) "issue issuerCopy id 1 by" (:id issuer-copy) "from" related-bnode "path" (str (:path hndq-state) id))
    (println (:recurse-level hndq-state) "replace issuerCopy", (:id issuer-copy) "with result issuer" (:id issuer))
    (-> hndq-state
        (update :path str id)           ; 5.4.5.2
        (update :path str "<" hash ">") ; 5.4.5.3
        (assoc :issuer-copy issuer)))) ; 5.4.5.4

(defn process-permutation
  [canon-state issuer hndq-state related-bnodes-permutation]
  #_(println (:recurse-level hndq-state) "permutation-0" related-bnodes-permutation issuer (pr-str hndq-state))

  (let [issuer-copy (clone-issuer issuer)
        ;; (or (:issuer-copy hndq-state) issuer) - TODO: is this `or` ok?
        _ (println (:recurse-level hndq-state) "clone issuer-copy from" (:id issuer) "to" (:id issuer-copy) "issued" (:issued issuer-copy))
        hndq-state (reduce (partial process-permutation-bnode1 canon-state) ; 5.4.4-5.4.4.2.2
                           (-> hndq-state
                               (assoc :issuer-copy issuer-copy) ; 5.4.1
                               (assoc :path "")                 ; 5.4.2
                               (assoc :recursion-list [])       ; 5.4.3
                               (assoc :next-perm false))
                           related-bnodes-permutation)]
    #_(println (:recurse-level hndq-state) "permutation-1" (pr-str hndq-state))
    (if (and (pos? (count (:chosen-path hndq-state)))
             (not (lex-less-than? (:path hndq-state) (:chosen-path hndq-state)))) ; 5.4.4.3
      hndq-state
      (let [hndq-state (reduce (partial process-permutation-bnode2 canon-state) ; 5.4.5.1-5.4.5.5
                               hndq-state
                               (:recursion-list hndq-state)) ; 5.4.5
            ;; _ (println (:recurse-level hndq-state) "permutation-2" (pr-str hndq-state))
            hndq-state (cond-> hndq-state
                         ;; 5.4.6
                         (or (zero? (count (:chosen-path hndq-state)))
                             (lex-less-than? (:path hndq-state) (:chosen-path hndq-state)))
                         (-> (assoc :chosen-path (:path hndq-state))
                             (assoc :chosen-issuer (:issuer-copy hndq-state))))]
        #_(println (:recurse-level hndq-state) "permutation-3" (pr-str hndq-state))
        (println (:recurse-level hndq-state) "replace chosenIssuer"  (:id (:chosen-issuer hndq-state)) "with issuerCopy" (:id (:issuer-copy hndq-state)))
        hndq-state))))

(defn process-related-bnodes
  [canon-state issuer hndq-state [related-hash related-bnodes]]
  #_(println (:recurse-level hndq-state) "related-bnodes-start" related-hash related-bnodes (pr-str hndq-state))
  (println (:recurse-level hndq-state) "chosenIssuer initialized" nil)
  (let [hndq-state (reduce (partial process-permutation canon-state issuer) ; 5.4.1-5.4.6
                           (-> hndq-state
                               (update :data-to-hash str related-hash) ; 5.1
                               (assoc :chosen-path "")                 ; 5.2
                               (assoc :chosen-issuer nil))             ; 5.3
                           (combo/permutations related-bnodes))]       ; 5.4
    #_(println (:recurse-level hndq-state) "related-bnodes-end" (pr-str hndq-state))
    (println (:recurse-level hndq-state) "replace issuer" (:id issuer) "with chosenIssuer" (:id (:chosen-issuer hndq-state)))
    (-> hndq-state
        (update :data-to-hash str (:chosen-path hndq-state))
        (assoc :issuer (:chosen-issuer hndq-state)))))

(defn hash-n-degree-quads
  [{:keys [canonical-issuer bnode->quad-info] :as canon-state} bnode issuer & [recurse-level]]
  ;; 4.8

  ;; 1. Create a hash to related blank nodes map for storing hashes that identify related blank nodes.
  ;; 2. Get a reference, quads, to the list of quads in the blank node to quads map for the key identifier.
  ;; 3. For each quad in quads:
  ;; 3.1 For each component in quad, where component is the subject, object, or graph name, and it is a blank
  ;;     node that is not identified by identifier:
  ;; 3.1.1 Set hash to the result of the Hash Related Blank Node algorithm, passing the blank node identifier for
  ;;       component as related, quad, path identifier issuer as issuer, and position as either s, o, or g based
  ;;       on whether component is a subject, object, graph name, respectively.
  ;; 3.1.2 Add a mapping of hash to the blank node identifier for component to hash to related blank nodes map,
  ;;       adding an entry as necessary.
  ;; 4. Create an empty string, data to hash.
  ;; 5. For each related hash to blank node list mapping in hash to related blank nodes map, sorted lexicographically by related hash:
  ;; 5.1 Append the related hash to the data to hash.
  ;; 5.2 Create a string chosen path.
  ;; 5.3 Create an unset chosen issuer variable.
  ;; 5.4 For each permutation of blank node list:
  ;; 5.4.1 Create a copy of issuer, issuer copy.
  ;; 5.4.2 Create a string path.
  ;; 5.4.3 Create a recursion list, to store blank node identifiers that must be recursively processed by this algorithm.
  ;; 5.4.4 For each related in permutation:
  ;; 5.4.4.1 If a canonical identifier has been issued for related, append it to path.
  ;; 5.4.4.2 Otherwise:
  ;; 5.4.4.2.1 If issuer copy has not issued an identifier for related, append related to recursion list.
  ;; 5.4.4.2.2 Use the Issue Identifier algorithm, passing issuer copy and related and append the result to path.
  ;; 5.4.4.3 If chosen path is not empty and the length of path is greater than or equal to the length of chosen path
  ;;         and path is lexicographically greater than chosen path, then skip to the next permutation.
  ;; 5.4.5 For each related in recursion list:
  ;; 5.4.5.1 Set result to the result of recursively executing the Hash N-Degree Quads algorithm, passing related for
  ;;         identifier and issuer copy for path identifier issuer.
  ;; 5.4.5.2 Use the Issue Identifier algorithm, passing issuer copy and related and append the result to path.
  ;; 5.4.5.3 Append <, the hash in result, and > to path.
  ;; 5.4.5.4 Set issuer copy to the identifier issuer in result.
  ;; 5.4.5.5 If chosen path is not empty and the length of path is greater than or equal to the length of chosen path
  ;;          and path is lexicographically greater than chosen path, then skip to the next permutation.
  ;; 5.4.6 If chosen path is empty or path is lexicographically less than chosen path, set chosen path to path
  ;;       and chosen issuer to issuer copy.
  ;; 5.5 Append chosen path to data to hash.
  ;; 5.6 Replace issuer, by reference, with chosen issuer.
  ;; 6 Return issuer and the hash that results from passing data to hash through the hash algorithm.
  (let [recurse-level (str recurse-level)
        hash->related-bnodes (map-hash-to-related-bnodes canon-state bnode issuer) ; 1-3.1.2
        _ (println recurse-level "hndq" (pr-str bnode) (:id issuer) (pr-str issuer))
        ;; _ (println recurse-level "hash->related-bnodes" (pr-str hash->related-bnodes))
        {:keys [data-to-hash issuer]}
        (reduce (partial process-related-bnodes canon-state issuer) ; 4-
                {:recurse-level recurse-level}
                (sort-by first hash->related-bnodes))] ; 5.
    (println recurse-level "RESULT" data-to-hash (pr-str issuer))
    {:hash (crypto/sha2-256 data-to-hash) :issuer issuer})) ; 5.6, 6 ; TODO: replace by ref? do I need to mutate?

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
                           (println "issue canon id 0 by" (:id canonical-issuer) "from" (first bnodes))
                           (assoc state :canonical-issuer canonical-issuer*)))) ; 5.4.3-5 elided
                     {:canonical-issuer canonical-issuer
                      :non-uniques []}))
        canonical-issuer
        (reduce (fn [canonical-issuer bnodes] ; 6
                  (let [hash-path-list  ; 6.1 - [{:hash "..." :issuer {...}}]
                        (reduce (fn [hash-path-list bnode] ; 6.2
                                  (println "id" bnode)
                                  (if (issued-id canonical-issuer bnode)
                                    hash-path-list ; 6.2.1
                                    (let [issuer (create-issuer "_:b") ; 6.2.2
                                          _ (println "create issuer" (:id issuer))
                                          [issuer*] (issue-id issuer bnode)] ; 6.2.3
                                      (println "issue issuer id by" (:id issuer) "from" bnode)
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
                                               (println "issue canon id 2 by" (:id canonical-issuer) "from" bnode)
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

;; 4.4 Canonicalization Algorithm
(defn canonicalize
  [quads]
  (def zzz quads)
  (let [canon-state (initialize-canonicalization-state quads) ; 1 - 5.3.2
        canonical-issuer (assign-canonical-ids canon-state)]  ; 5.4 - 6.3.1
    (->> quads                                                ; 7
         (map (partial replace-bnodes canonical-issuer))      ; 7.1, 7.2
         nquads/serialize)))                                  ; 8

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
