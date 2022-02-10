(ns fluree.json-ld.impl.util
  (:require #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.edn :as edn]))
  #?(:cljs (:require-macros [fluree.json-ld.impl.macros :refer [if-cljs try-catchall]])))

#?(:clj (set! *warn-on-reflection* true))


(defn sequential
  "Takes any value and if not sequential?, wraps it in a vector."
  [x]
  (if (sequential? x)
    x
    [x]))


(defn read-resource
  [filename]
  #?(:cljs (throw (ex-info (str "Loading external resources is not yet supported in Javascript.")
                           {:status 400 :error :json-ld/external-resource}))
     :clj  (try
             (some-> filename
                     io/resource
                     slurp
                     edn/read-string)
             (catch Exception e
               (throw (ex-info
                        (str "Invalid IRI, unable to read vocabulary from: " filename)
                        {:status 400 :error :json-ld/external-resource}
                        e))))))
