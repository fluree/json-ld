.PHONY: test jar install deploy clean

SOURCES := $(shell find src)

target/fluree-jsonld.jar: pom.xml deps.edn $(SOURCES)
	clojure -X:jar

pom.xml: deps.edn
	clojure -Spom

test-clj:
	clojure -M:test

test-nodejs:
	npx shadow-cljs release nodejs-test

test-browser:
	npx shadow-cljs release browser-test
	./node_modules/karma/bin/karma start --single-run

test-cljs: test-nodejs test-browser

test: test-clj test-cljs

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
