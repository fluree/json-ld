(ns fluree.json-ld.impl.util
  (:require #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])
            #?(:cljs ["fs" :as fs])
            #?(:cljs ["path" :as path]))
  #?(:cljs (:require-macros [fluree.json-ld.impl.util :refer [try-catchall if-cljs inline-resource]])))

#?(:clj
   (defmacro inline-resource
     "Macro allowing ClojureScript to inline a SMALL bundle of resource file(s) (< 1mb)
  at compile time.  If inline content grows, need to consider publishing to
  and downloading from a cdn."
     [resource-path]
     (slurp (io/resource resource-path))))



#?(:cljs
   (defn node-env?
     "Returns true if the runtime is a nodejs environment, false if a browser runtime."
     []
     (try (and js/Window false)
          (catch js/Error e
            true))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (defn cljs-env?
          "Take the &env from a macro, and tell whether we are expanding into cljs."
          [env]
          (boolean (:ns env))))

#?(:clj
   (defmacro if-cljs
     "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
     [then else]
     (if (cljs-env? &env) then else)))

#?(:clj
   (defmacro try-catchall
     "A cross-platform variant of try-catch that catches all exceptions.
   Does not (yet) support finally, and does not need or want an exception class."
     [& body]
     (let [try-body (butlast body)
           [catch sym & catch-body :as catch-form] (last body)]
       (assert (= catch 'catch))
       (assert (symbol? sym))
       `(if-cljs
            (try ~@try-body (~'catch js/Object ~sym ~@catch-body))
            (try ~@try-body (~'catch Throwable ~sym ~@catch-body))))))

(defn sequential
  "Takes any value and if not sequential?, wraps it in a vector."
  [x]
  (if (sequential? x)
    x
    [x]))

;; this is probably not a good approach for the browser, it makes for a very heavy dependency
#?(:cljs
   (do
     (def files
       {"contexts/fluree/ledger/v1.edn"
        (inline-resource "contexts/fluree/ledger/v1.edn")

        "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.edn"
        (inline-resource "contexts/org/imsglobal/purl/spec/clr/v1p0/context/clr_v1p0.edn")

        "contexts/org/w3/www/2018/credentials/v1.edn"
        (inline-resource "contexts/org/w3/www/2018/credentials/v1.edn")

        "contexts/org/w3/www/ns/did/v1.edn"
        (inline-resource "contexts/org/w3/www/ns/did/v1.edn")

        "contexts/org/w3id/security/v1.edn"
        (inline-resource "contexts/org/w3id/security/v1.edn")

        "contexts/org/w3id/security/v2.edn"
        (inline-resource "contexts/org/w3id/security/v2.edn")

        "contexts/org/schema/latest.edn"
        (inline-resource "contexts/org/schema/latest.edn")

        "contexts/org/geojson/geojson-ld/geojson-context.edn"
        (inline-resource "contexts/org/geojson/geojson-ld/geojson-context.edn")

        "org.schema.edn"
        (inline-resource "org.schema.edn")
        "owl.edn"
        (inline-resource "owl.edn")
        "rdfs.edn"
        (inline-resource "rdfs.edn")
        "skos.edn"
        (inline-resource "skos.edn")
        "org.purl.dc.terms.edn"
        (inline-resource "org.purl.dc.terms.edn")
        "org.w3id.openbadges.edn"
        (inline-resource "org.w3id.openbadges.edn")
        "org.imsglobal.spec.clr.vocab.edn"
        (inline-resource "org.imsglobal.spec.clr.vocab.edn")})))


(defn read-resource
  [filename]
  #?(:cljs
     (if-let [r (get files filename)]
       (edn/read-string r)
       (throw (ex-info (str "Invalid IRI, unable to read vocabulary from: " filename)
                       {:status 400 :error :json-ld/external-resource})))

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

(comment
  (try-catchall
    (/ 1 0)
    (catch e
        (println e)))

  (if-cljs :cljs :clj)
  :cljs
  :clj

  (fs/readdirSync (path/resolve js/__dirname (str "resources/")))

  (edn/read-string (fs.readFileSync "contexts/fluree/ledger/v1.edn" "utf-8"))

  (println "path:" (path/resolve js/__dirname))
  (println "resources:" (fs.readdirSync (path/resolve js/__dirname "resources/")))

  (read-resource "contexts/fluree/ledger/v1.edn")




  (try-catchall
    (read-resource "contexts/fluree/ledger/v1.edn")
    (catch e
        (println e)))

  (try (and js/Window false)
       (catch js/Error e
         true))
  (and js/Window false)

  (node-env?)


  ,)
