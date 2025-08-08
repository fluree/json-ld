#!/bin/bash

# Run GraalVM compatibility test for fluree/json-ld

set -e

echo "🧪 Running GraalVM compatibility test..."

PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$0")")")"
cd "$PROJECT_ROOT"


# Run the test with GraalVM dependencies
echo "Running Clojure test with GraalVM dependencies..."
clojure -Sdeps '{:paths ["src" "graalvm/test"]}' -M:dev:graalvm -e "(require 'graal-compat-test)" -e "(graal-compat-test/-main)"

echo "✅ GraalVM compatibility test completed successfully!"