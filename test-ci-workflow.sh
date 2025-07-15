#!/bin/bash

# Test script to verify the complete CI workflow locally
# This mimics what happens in GitHub Actions

set -e

echo "🧪 Testing complete CI workflow locally..."

echo "📦 Installing dependencies..."
npm ci || npm install

echo "🔨 Building JAR..."
make jar

echo "🔨 Building JavaScript packages..."
make js-package

echo "🧪 Running Clojure tests..."
make cljtest

echo "🧪 Running JavaScript ESM tests..."
make esm-test

echo "🔍 Running linter..."
make lint-ci

echo "📝 Checking code formatting..."
make fmt-check

echo "✅ All CI workflow steps completed successfully!"
echo ""
echo "📊 Summary:"
echo "✅ Dependencies installed"
echo "✅ JAR built"
echo "✅ JavaScript ESM modules built"
echo "✅ Clojure tests passed"
echo "✅ JavaScript ESM tests passed"
echo "✅ Linter checks passed"
echo "✅ Format checks passed"
echo ""
echo "🎉 Ready for CI/CD!"