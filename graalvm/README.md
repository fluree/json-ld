# GraalVM Native Image Support for fluree/json-ld

This directory contains configuration files, tests, and documentation for building fluree/json-ld as a GraalVM native image.

## Prerequisites

- **GraalVM 17+** with Native Image support installed
- **Clojure 1.12.1+** (already configured in this project)
- The `graal-build-time` dependency (already included via `:graalvm` alias)

## Quick Start

### 1. Test GraalVM Compatibility

```bash
# Run compatibility tests
./graalvm/test/run-graal-test.sh
```

### 2. Build Native Image

```bash
# Build JAR and compile to native image
./graalvm/configs/native-image.sh
```

### 3. Run Native Binary

```bash
# Test the generated native binary
./fluree-json-ld
```

## Project Structure

```
graalvm/
├── configs/
│   ├── reflect-config.json     # Reflection configuration
│   └── native-image.sh         # Native image build script
├── test/
│   ├── graal_compat_test.clj   # GraalVM compatibility tests
│   └── run-graal-test.sh       # Test runner script
└── README.md                   # This file
```

## Configuration Details

### Reflection Configuration

The `reflect-config.json` file includes reflection metadata for:

- **JSON API classes** (`javax.json.*`)
- **Glassfish JSON implementation** (`org.glassfish.json.*`)
- **Titanium JSON-LD processor** (`com.apicatalog.jsonld.*`)

### Native Image Features

The build includes the following GraalVM features:

- `clj_easy.graal_build_time.InitClojureClasses` - Proper Clojure class initialization
- HTTP/HTTPS protocol support for remote context loading
- Reflection configuration for JSON processing
- Exception stack trace reporting for debugging

## Memory Requirements

Native image compilation requires significant memory:

- **Minimum**: 4GB RAM (`-J-Xmx4g`)
- **Recommended**: 8GB+ RAM for large projects

## What's Been Tested

### Core API Functions ✅ Verified

All main fluree/json-ld API functions have been thoroughly tested in GraalVM:

- **`parse-context`** - Parses JSON-LD contexts into fluree's internal format
- **`expand-iri`** - Expands compact IRIs to full URIs  
- **`compact-fn`** - Creates compaction functions for URIs
- **`expand`** - Expands JSON-LD documents to fluree's internal format
- **`compact`** - Compacts expanded documents (basic functionality)
- **`normalize-data`** - Normalizes JSON-LD data for consistent formatting
- **`json-ld?`** - Detects JSON-LD documents
- **Core normalization** - Pure Clojure RFC 8785 implementation

### Test Coverage

**28 assertions** across **2 test suites** covering:
- Document structure validation
- Type system compatibility  
- Error-free execution
- Data format consistency
- IRI expansion/compaction round-trips

### Supported Features

- ✅ **JSON-LD normalization** (pure Clojure RFC 8785 implementation)
- ✅ **Context parsing and IRI expansion/compaction** 
- ✅ **JSON-LD document detection**
- ✅ **Document expansion** to fluree's internal format
- ✅ **All core fluree/json-ld functionality** (non-JavaScript)
- ✅ **Zero reflection warnings** in GraalVM environment


## Troubleshooting

### Future Development Considerations

- **Dynamic evaluation**: GraalVM restricts runtime code evaluation, which may limit certain dynamic features
- **Reflection**: While current reflection needs are fully configured, new reflective access patterns will require updating `reflect-config.json`

### Common Issues

**Build fails with memory errors:**
```bash
# Increase memory allocation
export NATIVE_IMAGE_OPTS="-J-Xmx8g"
```

**Missing reflection configuration:**
```bash
# Use GraalVM tracing agent to discover missing configuration
java -agentlib:native-image-agent=config-output-dir=graalvm/configs \
     -cp your-app.jar your.main.Class
```

**JSON processing errors:**
- Ensure all JSON API classes are included in `reflect-config.json`
- Check that the Glassfish JSON provider is accessible

### Debug Native Image Build

```bash
# Add debug flags to native-image.sh
native-image \
    -H:+PrintAnalysisCallTree \
    -H:+DashboardAll \
    -H:DashboardDump=build-dashboard \
    # ... other flags
```

## Building Upstream Libraries with GraalVM Support

### For Libraries Using fluree/json-ld

If your project depends on fluree/json-ld and wants GraalVM native image support:

#### 1. Add Dependencies

```clojure
;; In your deps.edn
{:deps {com.fluree/json-ld {:mvn/version "LATEST"}}
 
 :aliases
 {:graalvm 
  {:extra-deps {com.github.clj-easy/graal-build-time {:mvn/version "1.0.5"}}}}}
```

#### 2. Copy Reflection Configuration

Copy or reference fluree/json-ld's reflection configuration:

```bash
# Option 1: Copy the config
cp node_modules/fluree-json-ld/graalvm/configs/reflect-config.json graalvm/
```

```json
// Option 2: Merge into your reflect-config.json
// Include the JSON API classes from fluree/json-ld's config
```

#### 3. Update Your Native Image Build

```bash
# Add to your native-image command
native-image \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    -H:ReflectionConfigurationFiles=graalvm/reflect-config.json \
    # ... your other flags
```

#### 4. Test Your Integration

```clojure
;; Create a GraalVM compatibility test
(ns your-app.graal-test
  (:require [fluree.json-ld :as jld]
            [clojure.test :refer [deftest is testing]]))

(deftest graal-integration-test
  (testing "fluree/json-ld works in your GraalVM build"
    (let [doc {"@context" {"name" "http://schema.org/name"}
               "name" "Test"}
          parsed-context (jld/parse-context {"name" "http://schema.org/name"})
          normalized (jld/normalize-data doc)]
      (is (map? parsed-context))
      (is (string? normalized))
      (is (= "http://schema.org/name" (jld/expand-iri "name" parsed-context))))))
```

### Recommended Usage Pattern

```clojure
(ns your-app
  (:require [fluree.json-ld :as jld]                    ; ✅ Core functionality
            [fluree.json-ld.impl.normalize :as norm])   ; ✅ Pure Clojure
  ;; Avoid: fluree.json-ld.processor.api               ; ❌ Requires JavaScript
  )

;; ✅ GraalVM-compatible functions
(defn process-json-ld [data context-map]
  (let [parsed-context (jld/parse-context context-map)
        expanded (jld/expand data)
        normalized (jld/normalize-data data)]
    {:parsed-context parsed-context
     :expanded expanded
     :normalized normalized}))

;; ✅ IRI operations
(defn expand-and-compact-iri [compact-iri context-map]
  (let [parsed-context (jld/parse-context context-map)
        expanded (jld/expand-iri compact-iri parsed-context)
        compact-fn (jld/compact-fn parsed-context)]
    {:expanded expanded
     :compacted (compact-fn expanded)}))
```

### Advanced Configuration Tips

#### Memory Optimization

```bash
# For large JSON-LD context files, increase heap size
export NATIVE_IMAGE_OPTS="-J-Xmx6g"

# Enable class initialization at build time
-H:+InitializeAtBuildTime=fluree.json_ld
```

#### Debugging Reflection Issues

```bash
# Use tracing agent to discover missing reflection config
java -agentlib:native-image-agent=config-output-dir=graalvm/discovered \
     -cp target/your-app.jar \
     your.main.Class

# Compare with existing config
diff graalvm/reflect-config.json graalvm/discovered/reflect-config.json
```

#### Common Integration Patterns

```clojure
;; Pattern 1: Pre-parse contexts at startup
(def ^:const default-context
  (jld/parse-context {"name" "http://schema.org/name"
                      "Person" "http://schema.org/Person"}))

;; Pattern 2: Create reusable compaction functions  
(def compact-schema-org
  (jld/compact-fn default-context))

;; Pattern 3: Normalize data for consistent hashing
(defn content-hash [json-ld-data]
  (-> json-ld-data
      jld/normalize-data
      hash))
```

## How to Test GraalVM Compatibility

### Running the Test Suite

The GraalVM test suite includes **28 assertions** across **8 core API functions**:

```bash
# Run comprehensive GraalVM compatibility tests
./graalvm/test/run-graal-test.sh
```

**What gets tested:**
- `parse-context` - Context parsing with structure validation
- `expand-iri` - IRI expansion with round-trip verification  
- `compact-fn` - Compaction function creation and usage
- `expand` - Document expansion to internal format
- `compact` - Document compaction (basic functionality)
- `normalize-data` - Data normalization consistency
- `json-ld?` - Document detection logic
- `normalize` - Core RFC 8785 implementation

### Test Output

```
🧪 Running GraalVM compatibility tests for fluree/json-ld...

Testing graal-compat-test

Ran 2 tests containing 28 assertions.
0 failures, 0 errors.
✅ All GraalVM compatibility tests passed!
```

### Manual Testing

```clojure
;; Test individual functions manually
clojure -M:dev:test -e "
(require 'fluree.json-ld)
(def ctx (fluree.json-ld/parse-context {\"name\" \"http://schema.org/name\"}))
(println \"Context parsed:\" ctx)
(println \"IRI expanded:\" (fluree.json-ld/expand-iri \"name\" ctx))
"
```

### Testing in Upstream Projects

When integrating fluree/json-ld in your GraalVM project:

```clojure
;; Add to your test suite
(deftest our-graal-integration
  (testing "fluree/json-ld integration in our GraalVM build"
    (let [our-context {"title" "http://purl.org/dc/terms/title"
                       "author" "http://purl.org/dc/terms/creator"}
          parsed (jld/parse-context our-context)
          doc {"@context" our-context
               "title" "My Document" 
               "author" "Jane Doe"}
          normalized (jld/normalize-data doc)]
      ;; Verify parsing works
      (is (map? parsed))
      (is (contains? parsed "title"))
      
      ;; Verify IRI expansion
      (is (= "http://purl.org/dc/terms/title" 
             (jld/expand-iri "title" parsed)))
      
      ;; Verify normalization
      (is (string? normalized))
      (is (.contains normalized "Jane Doe")))))
```

### Performance Testing

```bash
# Test native image performance vs JVM
# Build native image first
./graalvm/configs/native-image.sh

# Compare startup times
time java -jar target/your-app.jar    # JVM version
time ./your-app-native                # Native version

# Memory usage comparison  
java -XX:+PrintGCDetails -jar target/your-app.jar
# vs native image (check with system tools)
```

### Debugging Test Failures

**Reflection errors:**
```clojure
;; Enable reflection warnings
(set! *warn-on-reflection* true)
;; Re-run tests to see specific warnings
```

**Missing classes:**
```bash
# Use tracing agent during tests
java -agentlib:native-image-agent=config-output-dir=test-trace \
     -cp $(clojure -Spath) clojure.main -m graal-compat-test
```

**Memory issues:**
```bash
# Increase memory for testing
export JAVA_OPTS="-Xmx4g"
./graalvm/test/run-graal-test.sh
```

## Resources

- [GraalVM Native Image Documentation](https://www.graalvm.org/latest/reference-manual/native-image/)
- [clj-easy/graal-docs](https://github.com/clj-easy/graal-docs)
- [Clojure GraalVM Best Practices](https://github.com/clj-easy/graal-docs#clojure-specific-recommendations)