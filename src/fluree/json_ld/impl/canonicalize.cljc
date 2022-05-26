(ns fluree.json-ld.impl.canonicalize
  (:require [clojure.string :as str]
            [instaparse.core :as grammar]
            [clojure.java.io :as io]
            [lambdaisland.regal :as reg]
            [lambdaisland.regal.parse :as reg-parse]
            [lambdaisland.regal.generator :as reg-gen]
            ))

(def g0 (slurp (io/resource "n-quads-grammar.ebnf")))


(def eol :line-break)
(def iri [:cat "<" [:capture [:+ [:not ":"]] ":" [:* [:not ">"]]] ">"])
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
(def string-literal-quote [:capture
                           [:cat
                            "\""
                            [:* [:alt
                                 [:not \u0022 \u005C \u000A \u000D]
                                 echar
                                 uchar]]
                            "\""]])

(def literal [:cat string-literal-quote
              [:? [:alt datatype lang-tag]]])

(def subject [:cat [:alt iri bnode] wsc])
(def predicate [:cat iri wsc])
(def object [:cat [:alt iri bnode literal] wso])
(def graph-name [:? iri])
(def quad [:cat :start subject predicate object graph-name "." :end])
(def doc [:cat [:? quad] [:* eol quad] [:? eol]])

(def empty-line [:cat :start wso :end])


(def g1
  "nquadsDoc ::= statement? (EOL statement)* EOL?
statement ::= subject predicate object graphLabel? '.'
subject  ::= IRIREF | BLANK_NODE_LABEL
predicate ::= IRIREF
object   ::= IRIREF | BLANK_NODE_LABEL | literal
graphLabel ::= IRIREF
literal  ::= STRING_LITERAL_QUOTE ('^^' IRIREF | '@' LANGTAG)?

LANGTAG  ::= #'@([a-zA-Z]+(?:-[a-zA-Z0-9]+)*)'
EOL      ::= #'(?:\\r\\n|(?!\\r\\n)[\\n-\\r\\x85\\u2028\\u2029])'
IRIREF   ::= #'<([^:]+:[^>]*)>'
HEX      ::= #'[0-9]|[A-F]|[a-f]'
UCHAR    ::= '\\u' HEX HEX HEX HEX | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX
ECHAR    ::= #'\\t|\\b|\\n|\\r|\\f'
STRING_LITERAL_QUOTE
         ::= '\"' ([^\\x22\\x5c\\x0a\\x0d] | ECHAR | UCHAR)* '\"'
BLANK_NODE_LABEL
         ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
PN_CHARS_BASE
         ::= [A-Z] | [a-z] | #'[\\u00C0-\\u00D6]' | #'[\\u00D8-\\u00F6]' | #'[\\u00F8-\\u02FF]' | #'[\\u0370-\\u037D]' | #'[\\u037F-\\u1FFF]' | #'[\\u200C-\\u200D]' | #'[\\u2070-\\u218F]' | #'[\\u2C00-\\u2FEF]' | #'[\\u3001-\\uD7FF]' | #'[\\uF900-\\uFDCF]' | #'[\\uFDF0-\\uFFFD]' | #'[\\u10000-\\uEFFFF]'
PN_CHARS_U
         ::= PN_CHARS_BASE | '_' | ':'
PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #'[\\xB7]' | #'[\\u0300-\\u036F]' | #'[\\u203F-\\u2040]'

")

(comment
  ;; STRING_LITERAL_QUOTE doesn't work with pattern or regex
  (def parse-nquads (grammar/parser g1))

  (re-find (reg/regex string-literal-quote) "\"1\"")
  "\"1\""


  ["\"1\"" "1"]
  nil

  (def in0 "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  (def in1 "<http://example.com> <http://example.com/label> \"test\"@en .")
  (def in2 "<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>.")
  (def in3 "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  (def in "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .
<http://example.com> <http://example.com/label> \"test\"@en .
<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>.
_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")

  (defn label
    [match]
    (map (partial vector) [:match :subject-iri :subject-bnode :predicate :object-iri :object-bnode :object-literal :datatype :lang-tag :graph :e1 :e2 :e3 :e4]
         match))

  (re-find (reg/regex string-literal-quote) in0)
  "\"test\""
  ["\"test\"^^<http://example.com/t1>" "http://example.com/t1" nil]


  (label (re-matches (reg/regex quad) in0)
         )
  ([:match
    "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> ."]
   [:subject-iri "http://example.com"]
   [:subject-bnode nil]
   [:predicate "http://example.com/label"]
   [:object-iri nil]
   [:object-bnode nil]
   [:object-literal "\"test\""]
   [:datatype "http://example.com/t1"]
   [:lang-tag nil]
   [:graph nil])

  (label (re-matches (reg/regex quad) in1))
  ([:match "<http://example.com> <http://example.com/label> \"test\"@en ."]
   [:subject-iri "http://example.com"]
   [:subject-bnode nil]
   [:predicate "http://example.com/label"]
   [:object-iri nil]
   [:object-bnode nil]
   [:object-literal "\"test\""]
   [:datatype nil]
   [:lang-tag "en"]
   [:graph nil])

  (label (re-matches (reg/regex quad) in2))
  ([:match
    "<http://example.com> <http://example.com/count> \"1\" <http://example.com/graphname>."]
   [:subject-iri "http://example.com"]
   [:subject-bnode nil]
   [:predicate "http://example.com/count"]
   [:object-iri nil]
   [:object-bnode nil]
   [:object-literal "\"1\""]
   [:datatype nil]
   [:lang-tag nil]
   [:graph "http://example.com/graphname"])


  (label (re-matches (reg/regex quad) in3))
  ([:match
    "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> ."]
   [:subject-iri nil]
   [:subject-bnode "_:b0"]
   [:predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
   [:object-iri "http://example.org/vocab#Foo"]
   [:object-bnode nil]
   [:object-literal nil]
   [:datatype nil]
   [:lang-tag nil]
   [:graph nil])






  (->> (map #(re-matches (reg/regex quad) %) (str/split-lines in))
       #_(map (partial map-indexed #(vector %1 %2)) ))




  (re-find (reg/regex [:capture bnode]) "_:b0")
  ["_:b0" "_:b0"]
  "_:b0"
  (re-find #"_:.*" "_:b0 ")
  "_:b0 "
  (re-find #"<([^:]+:[^>]*)>" "<_:b0> ")
  ["<_:b0>" "_:b0"]
  ni
  nil
  nil
  "_:b0 "
  ["_:b0 " nil]

  (re-matches (reg/regex subject) "<http://example.com> ")
  ["<http://example.com> " "http://example.com"]


  ["<http://example.com> " "http://example.com"]
  ["<http://example.com>" "http://example.com"]
  (re-find (reg/regex bnode) "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> .")
  "_:b0"
  ["_:b0 " nil]
  ["_:b0 " nil]
  "_:b0"
  ["_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> ." nil "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://example.org/vocab#Foo" nil nil nil nil]
  nil
  ("_:b0")
  "_:b0"
  ["_:b0 " nil]

  ["_:b0 " nil]
  "_:b0"


  (loop [[line & lines] (str/split in (reg/regex eol))
         dataset []]
    (cond (nil? line)
          dataset

          (re-matches (reg/regex empty-line) line)
          (recur lines dataset)

          (re-matches (reg/regex quad) line)
          (let [[match subject-iri subject-bnode predicate object-ref object-val datatype lang-tag graph-name] (re-matches (reg/regex quad) line)])
          ))
  (first ["<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> ." "<http://example.com> <http://example.com/label> \"test\"@en ." "<http://example.com> <http://example.com/count> 1 <http://example.com/graphname>." "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/vocab#Foo> ."])
  (re-matches (reg/regex quad) "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  nil
  (["<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> ."
    "http://example.com"
    "http://example.com/label"
    nil
    "test"
    "http://example.com/t1"
    nil
    nil])
  (re-find (reg/regex quad) "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  ["<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> ."
   "http://example.com"
   "http://example.com/label"
   nil
   "test"
   "http://example.com/t1"
   nil
   nil]
  (re-matches (reg/regex quad) "<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> .")
  ["<http://example.com> <http://example.com/label> \"test\"^^<http://example.com/t1> ."
   "http://example.com"
   "http://example.com/label"
   nil
   "test"
   "http://example.com/t1"
   nil
   nil]


  ["<http://example.com>
<http://example.com/label>
\"test\"^^<http://example.com/t1> ."
   "http://example.com" "http://example.com/label" nil "test" "http://example.com/t1" nil nil]
  (reg/regex quad)





  ,)
