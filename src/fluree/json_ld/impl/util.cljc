(ns fluree.json-ld.impl.util
  (:require #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.edn :as edn])))

#?(:clj (set! *warn-on-reflection* true))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))


(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))


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
       (try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))


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
