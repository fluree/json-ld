.PHONY: test jar install deploy clean

SOURCES := $(shell find src)

target/fluree-jsonld.jar: pom.xml deps.edn $(SOURCES)
	clojure -X:jar

pom.xml: deps.edn
	clojure -Spom

test:
	clojure -M:test

jar: target/fluree-json-ld.jar

install: target/fluree-json-ld.jar
	clojure -X:install

# You'll need to set the env vars CLOJARS_USERNAME & CLOJARS_PASSWORD
# (which must be a Clojars deploy token now) to use this.
deploy: target/fluree-json-ld.jar
	clojure -X:deploy

clean:
	rm -rf target