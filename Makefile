.PHONY: test jar install deploy clean edn-contexts

SOURCES := $(shell find src)

target/fluree-json-ld.jar: pom.xml deps.edn src/deps.cljs $(SOURCES)
	clojure -X:jar

src/deps.cljs: package.json
	clojure -M:js-deps

resources/contexts/%.edn: resources/contexts/%.jsonld
	clojure -X:build-edn-context :source '"$(subst resources/,,$<)"' :dest '"$(subst resources/,,$@)"'

CONTEXTS := $(shell find resources/contexts -name '*.jsonld')
EDN_CONTEXTS := $(CONTEXTS:.jsonld=.edn)

edn-contexts: $(EDN_CONTEXTS)

pom.xml: deps.edn
	clojure -Spom

cljtest:
	clojure -X:test

nodetest:
	npx shadow-cljs release nodejs-test

browsertest:
	npx shadow-cljs release browser-test
	./node_modules/karma/bin/karma start --single-run

cljstest: nodetest browsertest

test: cljtest cljstest

jar: target/fluree-json-ld.jar

install: target/fluree-json-ld.jar
	clojure -X:install

# You'll need to set the env vars CLOJARS_USERNAME & CLOJARS_PASSWORD
# (which must be a Clojars deploy token now) to use this.
deploy: target/fluree-json-ld.jar
	clojure -X:deploy

clean:
	rm -rf target
	rm -rf node_modules
	rm -rf test/nodejs
	rm -rf test/browser
