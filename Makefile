.PHONY: test jar install deploy clean

test:
	clojure -A:test:runner

pom.xml: deps.edn
	clojure -Spom

target/json-ld.jar: pom.xml src/**/* resources/**/*
	clojure -A:jar

jar: target/json-ld.jar

install: target/json-ld.jar
	clojure -A:install

# requires CLOJARS_USERNAME and CLOJARS_PASSWORD env vars to be set
deploy: target/json-ld.jar
	clojure -A:deploy

clean:
	rm -rf target