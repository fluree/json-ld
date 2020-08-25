(ns com.fluree.json-ld
  #?(:clj (:import (com.apicatalog.jsonld JsonLd)
                   (com.apicatalog.jsonld.api.impl ExpansionApi CompactionApi))))
#?(:clj
   (defn expand
     "JSON-LD expansion; returns an ExpansionApi instance"
     [^String location]
     (JsonLd/expand location)))

#?(:clj
   (defn expansion-base
     "Sets the base IRI for an ExpansionApi instance; returns the instance"
     [^ExpansionApi expansion ^String base]
     (.base expansion base)))

#?(:clj
   (defn expansion->json
     "Returns the javax.json.JsonArray representation of the given ExpansionApi instance"
     [^ExpansionApi expansion]
     (.get expansion)))

#?(:clj
   (defn compact
     "JSON-LD compaction; returns a CompactionApi instance"
     [^String doc-location ^String context-location]
     (JsonLd/compact doc-location context-location)))

#?(:clj
   (defn compaction-base
     "Sets the base IRI for a CompactionApi instance"
     [^CompactionApi compaction ^String base]
     (.base compaction base)))

#?(:clj
   (defn compaction->json
     "Returns the javax.json.JsonObject representation of the given CompactionApi instance"
     [^CompactionApi compaction]
     (.get compaction)))
