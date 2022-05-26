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
  [match]
  (let [{:keys [statement subject-iri subject-bnode predicate-iri
                object-iri object-bnode object-literal datatype lang-tag
                graph-iri graph-bnode]}
        (->> match
             (map (partial vector) capture-groups)
             (into {}))]
    {:statement statement
     :subject   (cond subject-iri   {:type :named :value subject-iri}
                      subject-bnode {:type :blank :value subject-bnode})
     :predicate {:type :named :value predicate-iri}
     :object    (cond object-iri
                      {:type :named :value object-iri
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})}

                      object-bnode
                      {:type :blank :value object-bnode
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})}

                      object-literal
                      {:type :literal :value object-literal
                       :datatype
                       (cond datatype {:type :named :value datatype}
                             lang-tag {:type :named :value RDF_LANGSTRING :language lang-tag}
                             :else    {:type :named :value XSD_STRING})})
     :graph {:type  (cond graph-iri   :named
                          graph-bnode :blank
                          :else       :default)
             :value (or graph-iri graph-bnode "")}}))

(defn parse
  [doc]
  (loop [[line & lines] (str/split doc (reg/regex eol))
         dataset #{}]
    (cond (nil? line)
          dataset

          (re-matches (reg/regex empty-line) line)
          (recur lines dataset)

          (re-matches (reg/regex quad) line)
          (let [quad (->quad (re-matches (reg/regex quad) line))]
            (recur lines (conj dataset quad))))))

(comment
  #_(def parse-nquads (grammar/parser g1))

  (re-find (reg/regex string-literal-quote) "\"1\"")


  (def in0 "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  (def in1 "<http://example.com> <http://example.com/label> \"test\"@en .")
  (def in2 "<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>.")
  (def in3 "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  (def in "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .
<http://example.com> <http://example.com/label> \"test\"@en .
<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>.
_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")


  {:subject {:type [:named :blank]
             :value ""}
   :predicate {:type :named
               :value ""}
   :object {:type [:named :blank :literal]
            :value ""
            :datatype {:type [:named "rdf-langstring" "xsd-string"]
                       :value ""
                       :language ""}}
   :graph {:type [:named :blank :default]
           :value ""}}


  (re-find (reg/regex [:*? rdf-comment]) "<a.com/foo#thing>  # comment")

  (->quad (re-matches (reg/regex quad) in0))
  (->quad (re-matches (reg/regex quad) in1))
  (->quad (re-matches (reg/regex quad) in2))
  (->quad (re-matches (reg/regex quad) in3))

  (re-find (reg/regex string-literal-quote) "\"hey\"")


  (parse in)
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
