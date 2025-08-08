#!/bin/bash

# GraalVM Native Image build script for fluree/json-ld
# Requires GraalVM 17+ and clj-easy/graal-build-time dependency

set -e

echo "Building GraalVM native image for fluree/json-ld..."

# Configuration directory
CONFIG_DIR="$(dirname "$0")"
PROJECT_ROOT="$(dirname "$(dirname "$0")")"

# Build the JAR first
echo "Building JAR..."
cd "$PROJECT_ROOT"
clojure -M:graalvm -T:build jar

# Find the built JAR
JAR_FILE=$(find target -name "*.jar" | head -1)
if [ -z "$JAR_FILE" ]; then
    echo "Error: No JAR file found in target directory"
    exit 1
fi

echo "Using JAR: $JAR_FILE"

# Native image build
echo "Building native image..."
native-image \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    -H:ReflectionConfigurationFiles="$CONFIG_DIR/reflect-config.json" \
    -H:+ReportExceptionStackTraces \
    -H:+PrintClassInitialization \
    -H:Log=registerResource: \
    -H:EnableURLProtocols=http,https \
    --enable-preview \
    --no-fallback \
    -J-Xmx4g \
    -jar "$JAR_FILE" \
    fluree-json-ld

echo "Native image build complete: fluree-json-ld"
echo "Test with: ./fluree-json-ld"