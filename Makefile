.PHONY: help test jar install deploy clean edn-contexts parse-all-contexts lint lint-ci fmt fmt-check cljtest cljstest cljs-node-test cljs-browser-test docker-build docker-test

SOURCES := $(shell find src)

.PHONY: help
help: ## Describe available tasks
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

target/fluree-json-ld.jar: deps.edn src/deps.cljs $(SOURCES)
	clojure -T:build jar

src/deps.cljs: package.json
	clojure -M:js-deps

resources/contexts/%.edn: resources/contexts/%.jsonld
	clojure -X:build-edn-context :source '"$(subst resources/,,$<)"' :dest '"$(subst resources/,,$@)"'

CONTEXTS := $(shell find resources/contexts -name '*.jsonld')
EDN_CONTEXTS := $(CONTEXTS:.jsonld=.edn)

edn-contexts: $(EDN_CONTEXTS) ## Build EDN versions of JSON-LD context files

# Re-parse all JSON-LD contexts from external.cljc
parse-all-contexts: ## Re-parse all JSON-LD contexts from external sources
	@echo "Re-parsing all JSON-LD contexts..."
	@clojure -M:dev scripts/parse-contexts.clj

pom.xml: deps.edn
	clojure -Spom

cljtest: ## Run Clojure tests
	clojure -X:test

cljs-node-test: ## Run ClojureScript Node.js tests
	npx shadow-cljs release nodejs-test

cljs-browser-test: ## Run ClojureScript browser tests with Karma
	npx shadow-cljs release browser-test
	./node_modules/karma/bin/karma start --single-run

node_modules: package.json
	npm install

cljstest: node_modules cljs-node-test cljs-browser-test ## Run all ClojureScript tests

test: cljtest cljstest ## Run all tests (Clojure and ClojureScript)

lint: ## Run clj-kondo linter
	clj-kondo --lint src test

lint-ci: ## Run clj-kondo linter with CI configuration
	clj-kondo --config .clj-kondo/ci-config.edn --lint src test

fmt: ## Fix code formatting with cljfmt
	clojure -M:fmt fix

fmt-check: ## Check code formatting with cljfmt
	clojure -M:fmt check

jar: target/fluree-json-ld.jar ## Build JAR file

install: target/fluree-json-ld.jar ## Install JAR to local repository
	clojure -T:build install

# You'll need to set the env vars CLOJARS_USERNAME & CLOJARS_PASSWORD
# (which must be a Clojars deploy token now) to use this.
deploy: target/fluree-json-ld.jar ## Deploy JAR to Clojars
	clojure -T:build deploy

clean: ## Remove build artifacts
	rm -rf target
	rm -rf node_modules
	rm -rf test/nodejs
	rm -rf test/browser

docker-build: ## Build Docker image for testing
	docker build -t fluree/json-ld:test .

docker-test: docker-build ## Run tests in Docker container
	docker run --rm --security-opt seccomp=docker-chrome-seccomp.json fluree/json-ld:test