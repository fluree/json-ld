(ns fluree.json-ld.impl.normalize-test
  (:require [clojure.test :refer [deftest is testing]]
            [fluree.json-ld.impl.normalize :as normalize]
            [clojure.string :as str]))

;; basic normalization per JSON Canonicalization Scheme https://datatracker.ietf.org/doc/html/rfc8785
;; following tests come from examples at https://cyberphone.github.io/doc/security/browser-json-canonicalization.html

(deftest data-as-map
  (testing "Map with some non-standard characters"
    (let [data {"peach" "This sorting order",
                "péché" "is wrong according to French",
                "pêche" "but canonicalization MUST",
                "sin"   "ignore locale"}]
      (is (= "{\"peach\":\"This sorting order\",\"péché\":\"is wrong according to French\",\"pêche\":\"but canonicalization MUST\",\"sin\":\"ignore locale\"}"
             (normalize/normalize data {:algorithm :basic
                              :format    :application/json})))))

  (testing "Map with line break as string and decimal to integer"
    (let [data {"1"   {"f" {"f" "hi", "F" 5}, "\n" 56.0},   ;; note 56.0 should become "56"
                "10"  {},
                ""    "empty",
                "a"   {},
                "111" [{"e" "yes", "E" "no"}],
                "A"   {}}]
      (is (= "{\"\":\"empty\",\"1\":{\"\n\":56,\"f\":{\"F\":5,\"f\":\"hi\"}},\"10\":{},\"111\":[{\"E\":\"no\",\"e\":\"yes\"}],\"A\":{},\"a\":{}}"
             (normalize/normalize data {:algorithm :basic
                              :format    :application/json})))))

  (testing "Unicode in string"
    (let [data {"Unnormalized Unicode" "A\u030a"}]
      (is (= "{\"Unnormalized Unicode\":\"Å\"}"
             (normalize/normalize data {:algorithm :basic
                              :format    :application/json})))))

  (testing "Numbers in different formats and literals"
    (let [data {"numbers"  [333333333.33333329, 1E30, 4.50, 2e-3, 0.000000000000000000000000001],
                "literals" [nil, true, false]}]
      (is (= "{\"literals\":[null,true,false],\"numbers\":[333333333.3333333,1e+30,4.5,0.002,1e-27]}"
             (normalize/normalize data {:algorithm :basic
                              :format    :application/json})))))

  ;; Note below fails but should not. Most unicode control set characters (< \u000f) should remain as is
  ;; but with lower case hex. Below, \u000F gets parsed but should instead output \u000f
  ;; see https://datatracker.ietf.org/doc/html/rfc8785#section-3.2.2.2 bullet #2
  ;; Need to figure out a way to efficiently detect and parse this, including in JS
  #_(testing "Unicode control set characters remain (but with lowercase hex), numbers, literals"
      (let [data {"numbers"  [333333333.33333329, 1E30, 4.50, 2e-3, 0.000000000000000000000000001],
                  "string"   "\u20ac$\u000F\u000aA'\u0042\u0022\u005c\\\"/",
                  "literals" [nil, true, false]}]
        (is (= "{\"literals\":[null,true,false],\"numbers\":[333333333.3333333,1e+30,4.5,0.002,1e-27],\"string\":\"€$\\u000f\\nA'B\\\"\\\\\\\\\\\"/\"}"
               (normalize data {:algorithm :basic}))))))


(deftest data-as-sequence
  (testing "Sequence that includes a map"
    (let [data [56 {"d"  true
                    "10" nil
                    "1"  []}]]
      (is (= "[56,{\"1\":[],\"10\":null,\"d\":true}]"
             (normalize/normalize data {:algorithm :basic
                              :format    :application/json}))))))

(def utf8-bytes [0x7b 0x22 0x70 0x65 0x61 0x63 0x68 0x22 0x3a
                 0x22 0x54 0x68 0x69 0x73 0x20 0x73 0x6f 0x72
                 0x74 0x69 0x6e 0x67 0x20 0x6f 0x72 0x64 0x65
                 0x72 0x22 0x2c 0x22 0x70 0xc3 0xa9 0x63 0x68
                 0xc3 0xa9 0x22 0x3a 0x22 0x69 0x73 0x20 0x77
                 0x72 0x6f 0x6e 0x67 0x20 0x61 0x63 0x63 0x6f
                 0x72 0x64 0x69 0x6e 0x67 0x20 0x74 0x6f 0x20
                 0x46 0x72 0x65 0x6e 0x63 0x68 0x22 0x2c 0x22
                 0x70 0xc3 0xaa 0x63 0x68 0x65 0x22 0x3a 0x22
                 0x62 0x75 0x74 0x20 0x63 0x61 0x6e 0x6f 0x6e
                 0x69 0x63 0x61 0x6c 0x69 0x7a 0x61 0x74 0x69
                 0x6f 0x6e 0x20 0x4d 0x55 0x53 0x54 0x22 0x2c
                 0x22 0x73 0x69 0x6e 0x22 0x3a 0x22 0x69 0x67
                 0x6e 0x6f 0x72 0x65 0x20 0x6c 0x6f 0x63 0x61
                 0x6c 0x65 0x22 0x7d])

(deftest roundtrip
  (testing "Using original UTF-8 bytes to construct json, parse, normalize, back to UTF-8"
    ;; {"peach":"This sorting order","péché":"is wrong according to French","pêche":"but canonicalization MUST","sin":"ignore locale"}
    (let [utf-8      #?(:clj (byte-array utf8-bytes)
                        :cljs (js/Uint8Array. utf8-bytes))
          to-str     #?(:clj (String. utf-8 "UTF-8")
                        :cljs (reduce str (map js/String.fromCharCode utf-8)))
          ;; poor man's json parser - avoiding extra dependency, only works for simple map
          parsed     (-> to-str
                         (subs 1 (dec (count to-str))) ;; remove leading/ending '{}'
                         (str/replace #"\"" "")
                         (str/split #",")
                         (->> (mapv #(str/split % #":"))
                              (into {})))
          normalized (normalize/normalize parsed {:algorithm :basic
                                                  :format    :application/json})
          utf-8*     #?(:clj (.getBytes ^String normalized "UTF-8")
                        :cljs (map #(.charCodeAt %) normalized))]
      (is (= (vec utf-8)
             (vec utf-8*))))))

;; Following also fails and needs to get addressed
#_(deftest weird-json
    (testing "weird.json example at "
      ;; {"\n":"Newline","\r":"Carriage Return","1":"One","</script>":"Browser Challenge","□":"Control","ö":"Latin Small Letter O With Diaeresis","€":"Euro Sign","😂":"Smiley","דּ":"Hebrew Letter Dalet With Dagesh"}
      (let [data   {"\u20ac"       "Euro Sign",
                    "\r"           "Carriage Return",
                    "\u000a"       "Newline",
                    "1"            "One",
                    "\u0080"       "Control\u007f",
                    "\ud83d\ude02" "Smiley",
                    "\u00f6"       "Latin Small Letter O With Diaeresis",
                    "\ufb33"       "Hebrew Letter Dalet With Dagesh",
                    "</script>"    "Browser Challenge"}
            utf-8  (byte-array [0x7b 0x22 0x5c 0x6e 0x22 0x3a 0x22 0x4e 0x65
                                0x77 0x6c 0x69 0x6e 0x65 0x22 0x2c 0x22 0x5c
                                0x72 0x22 0x3a 0x22 0x43 0x61 0x72 0x72 0x69
                                0x61 0x67 0x65 0x20 0x52 0x65 0x74 0x75 0x72
                                0x6e 0x22 0x2c 0x22 0x31 0x22 0x3a 0x22 0x4f
                                0x6e 0x65 0x22 0x2c 0x22 0x3c 0x2f 0x73 0x63
                                0x72 0x69 0x70 0x74 0x3e 0x22 0x3a 0x22 0x42
                                0x72 0x6f 0x77 0x73 0x65 0x72 0x20 0x43 0x68
                                0x61 0x6c 0x6c 0x65 0x6e 0x67 0x65 0x22 0x2c
                                0x22 0xc2 0x80 0x22 0x3a 0x22 0x43 0x6f 0x6e
                                0x74 0x72 0x6f 0x6c 0x7f 0x22 0x2c 0x22 0xc3
                                0xb6 0x22 0x3a 0x22 0x4c 0x61 0x74 0x69 0x6e
                                0x20 0x53 0x6d 0x61 0x6c 0x6c 0x20 0x4c 0x65
                                0x74 0x74 0x65 0x72 0x20 0x4f 0x20 0x57 0x69
                                0x74 0x68 0x20 0x44 0x69 0x61 0x65 0x72 0x65
                                0x73 0x69 0x73 0x22 0x2c 0x22 0xe2 0x82 0xac
                                0x22 0x3a 0x22 0x45 0x75 0x72 0x6f 0x20 0x53
                                0x69 0x67 0x6e 0x22 0x2c 0x22 0xf0 0x9f 0x98
                                0x82 0x22 0x3a 0x22 0x53 0x6d 0x69 0x6c 0x65
                                0x79 0x22 0x2c 0x22 0xef 0xac 0xb3 0x22 0x3a
                                0x22 0x48 0x65 0x62 0x72 0x65 0x77 0x20 0x4c
                                0x65 0x74 0x74 0x65 0x72 0x20 0x44 0x61 0x6c
                                0x65 0x74 0x20 0x57 0x69 0x74 0x68 0x20 0x44
                                0x61 0x67 0x65 0x73 0x68 0x22 0x7d])
            to-str (String. utf-8 "UTF-8")]
        (is (= to-str
               (normalize data {:algorithm :basic})))
        )))
