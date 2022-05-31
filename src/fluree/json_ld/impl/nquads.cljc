(ns fluree.json-ld.impl.nquads
  (:require [clojure.string :as str]
            [lambdaisland.regal :as reg]))

(def RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def RDF_LANGSTRING (str RDF "langString"))
(def XSD_STRING "http://www.w3.org/2001/XMLSchema#string")

(def eol :line-break)
(def iri [:cat "<" [:capture [:+ [:not ":"]] ":" [:* [:not ">"]]] ">"])
(def rdf-comment [:cat "#" [:* :any] :end])
(def ws [:class " " :tab])
(def wso [:* ws])
(def wsc [:+ ws])
(def pn-chars-base [:alt
                    [:class [\A \Z]]
                    [:class [\a \z]]
                    [:class [\u00C0 \u00D6]]
                    [:class [\u00D8 \u00F6]]
                    [:class [\u00F8 \u02FF]]
                    [:class [\u0370 \u037D]]
                    [:class [\u037F \u1FFF]]
                    [:class [\u200C \u200D]]
                    [:class [\u2070 \u218F]]
                    [:class [\u2C00 \u2FEF]]
                    [:class [\u3001 \uD7FF]]
                    [:class [\uF900 \uFDCF]]
                    [:class [\uFDF0 \uFFFD]]
                    #_[:class ["\\u10000" "\\uEFFFF"]]])
(def pn-chars-u [:alt pn-chars-base "_" ":"])
(def pn-chars [:alt pn-chars-u "-" [:class [\0 \9]] \u00B7 [:class [\u0300 \u036F]] [:class [\u203F \u2040]]])

(def bnode [:capture
            [:cat "_:"
             [:alt pn-chars-u [:class [\0 \9]]]
             [:? [:cat [:* [:alt pn-chars "."]] pn-chars ]]]])
(def lang-tag [:cat "@"
               [:capture
                [:+ [:class ["a" "z"] ["A" "Z"]]]
                [:* "-"
                 [:+ [:class ["a" "z"] ["A" "Z"] ["0" "9"]]]]]])
(def datatype [:cat "^^" iri])

(def hex [:alt [:class [\0 \9]] [:class [\A \F]] [:class [\a \f]]])
(def uchar [:alt
            [:cat "\\u" hex hex hex hex]
            [:cat "\\U" hex hex hex hex hex hex hex hex]])
(def echar [:alt :tab "\\b" :newline :return :form-feed])
(def string-literal-quote [:cat
                           "\""
                           [:capture
                            [:* [:alt
                                 [:not \u0022 \u005C \u000A \u000D]
                                 echar
                                 uchar]]]
                           "\""])

(def literal [:cat string-literal-quote
              [:? [:alt datatype lang-tag]]])

(def subject [:cat [:alt iri bnode] wsc])
(def predicate [:cat iri wsc])
(def object [:cat [:alt iri bnode literal] wso])
(def graph-name [:cat [:? [:alt iri bnode]] wso])
(def quad [:cat :start subject predicate object graph-name "." :end])
(def doc [:cat [:? quad] [:* eol quad] [:? eol]])

(def empty-line [:cat :start wso :end])


(def capture-groups
  "These are the names, in order, of the capture groups in the `quad` regex."
  [:statement :subject-iri :subject-bnode :predicate-iri :object-iri :object-bnode :object-literal
   :datatype :lang-tag :graph-iri :graph-bnode])

(defn ->quad
  "Takes an N-Quad string and turns it into an RDF quad."
  [statement]
  (let [match (re-matches (reg/regex quad) statement)
        {:keys [statement subject-iri subject-bnode predicate-iri
                object-iri object-bnode object-literal datatype lang-tag
                graph-iri graph-bnode]}
        (->> match
             (map (partial vector) capture-groups)
             (into {}))]
    {:statement statement
     :subject   (cond subject-iri   {:type :named :value subject-iri :term :subject}
                      subject-bnode {:type :blank :value subject-bnode :term :subject})
     :predicate {:type :named :value predicate-iri :term :predicate}
     :object    (cond object-iri
                      {:type :named :value object-iri :term :object
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})}

                      object-bnode
                      {:type :blank :value object-bnode :term :object
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})}

                      object-literal
                      {:type :literal :term :object :value object-literal ;TODO: unescape?
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})})
     :graph {:type  (cond graph-iri   :named
                          graph-bnode :blank
                          :else       :default)
             :term :graph
             :value (or graph-iri graph-bnode "")}}))

(defn ->statement
  "Takes an RDF quad and turns it into an N-Quad string."
  [{:keys [subject predicate object graph]}]
  (let [s (condp = (:type subject)
            :named (str "<" (:value subject) ">")
            :blank (:value subject))
        p (str "<" (:value predicate) ">")
        o (condp = (:type object)
            :named   (str "<" (:value object) ">")
            :blank   (:value object)
            :literal (str "\"" (:value object) "\""
                          (condp = (:value (:datatype object))
                            RDF_LANGSTRING (str "@" (:language (:datatype object)))
                            XSD_STRING     ""
                            ;; not xsd-string nor langstring
                            (str "^^" "<" (:value (:datatype object)) ">"))))
        g (condp = (:type graph)
            :named   (str "<" (:value graph) ">")
            :blank   (:value graph)
            :default nil)]

    (str s " " p " " o " " g (when g " ") ".")))

(defn parse
  "Turn an n-quads document into quads."
  [doc]
  (loop [[line & lines] (str/split doc (reg/regex eol))
         dataset #{}]
    (cond (nil? line)
          dataset

          (re-matches (reg/regex empty-line) line)
          (recur lines dataset)

          (re-matches (reg/regex quad) line)
          (let [quad (->quad line)]
            (recur lines (conj dataset quad))))))

(defn serialize
  "Turn a sorted set of quads into an n-quads string."
  [dataset]
  (str/join "\n" (map ->statement dataset)))

(comment
  #_(def parse-nquads (grammar/parser g1))

  (re-find (reg/regex string-literal-quote) "\"1\"")


  (def in0 "<http://example.com/1> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  (def in1 "<http://example.com/1> <http://example.com/label> \"test\"@en .")
  (def in2 "<http://example.com/1> <http://example.com/friend> _:b1 .")
  (def in3 "<http://example.com/2> <http://example.com/friend> <http://example.com/1> .")
  (def in4 "_:b1 <http://example.com/friend> _:b0 _:b3 .")
  (def in5"<http://example.com/2> <http://example.com/count> \"1\" <http://example.com/graphname> .")
  (def in6 "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  (def in (str/join "\n" [in0 in1 in2 in3 in4 in5 in6]))



  (re-find (reg/regex [:*? rdf-comment]) "<a.com/foo#thing>  # comment")


  (re-matches (reg/regex quad) in4)

  ["<http://example.com/1> <http://example.com/friend> _:b1 ." "http://example.com/1" nil "http://example.com/friend" nil "_:b1" nil nil nil nil nil]
  ["<http://example.com/1> <http://example.com/friend> _:b1 ." "http://example.com/1" nil "http://example.com/friend" nil "_:b1" nil nil nil nil nil]
  nil
  nil



  (re-matches (reg/regex quad) "<a.com/foo> <a.com/foo> <a.com/foo> <a.com/foo> .")
  nil
  nil
  nil
  nil
  nil
  nil
  nil
  nil

  ["_:b1 " nil "_:b1"]
  ["_:b1 " nil "_:b1" nil nil nil]
  ["<http://example.com/friend> " "http://example.com/friend"]
  ["_:b1 " nil "_:b1"]


  ["_:b1 " nil "_:b1"]
  ["_:b1 " nil "_:b1" nil nil nil]
  ["_:b1 " nil "_:b1"]
  ["_:b1" "_:b1"]

  (->quad (re-matches (reg/regex quad) in0))
  (->quad (re-matches (reg/regex quad) in1))
  (->quad (re-matches (reg/regex quad) in2))
  (->quad (re-matches (reg/regex quad) in3))


  (re-find (reg/regex string-literal-quote) "\"hey\"")


  (parse in)
  (map (partial filter (fn [q] (= :blank (:type (second q))))) (parse in))
  (() ([:subject {:type :blank, :value "_:b0"}]) () ())
  ([:subject {:type :blank, :value "_:b0"}])
  (() ([:subject {:type :blank, :value "_:b0"}]) () ())



  #{{:statement "<http://example.com> <http://example.com/label> \"test\"@en .",
     :subject {:type :named, :value "http://example.com"},
     :predicate {:type :named, :value "http://example.com/label"},
     :object
     {:type :literal,
      :value "test",
      :datatype
      {:type :named,
       :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString",
       :language "en"}},
     :graph {:type :default, :value ""}}

    {:statement
     "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .",
     :subject {:type :blank, :value "_:b0"},
     :predicate
     {:type :named, :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"},
     :object
     {:type :named,
      :value "http://example.org/vocab#Foo",
      :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
     :graph {:type :default, :value ""}}
    {:statement
     "<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>.",
     :subject {:type :named, :value "http://example.com"},
     :predicate {:type :named, :value "http://example.com/count"},
     :object
     {:type :literal,
      :value "1",
      :datatype {:type :named, :value "http://www.w3.org/2001/XMLSchema#string"}},
     :graph {:type :named, :value "http://example.com/graphname"}}
    {:statement
     "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .",
     :subject {:type :named, :value "http://example.com"},
     :predicate {:type :named, :value "http://example.com/label"},
     :object
     {:type :literal,
      :value "test",
      :datatype {:type :named, :value "http://example.com/t1"}},
     :graph {:type :default, :value ""}}}


  ,)
