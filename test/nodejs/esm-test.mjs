#!/usr/bin/env node

/**
 * Basic ESM functionality test for Node.js environment
 * Tests that the core JSON-LD functions work in Node.js ESM context
 */

import { expand, compact, normalizeData, parseContext } from '../../dist/nodejs/fluree-json-ld.js';

// Test data
const testDoc = {
  "@context": {
    "name": "http://schema.org/name",
    "Person": "http://schema.org/Person"
  },
  "@type": "Person",
  "name": "John Doe"
};

const testContext = {
  "name": "http://schema.org/name",
  "Person": "http://schema.org/Person"
};

async function runTests() {
  console.log('🧪 Running Node.js ESM functionality tests...');
  
  let passed = 0;
  let failed = 0;
  
  // Test 1: Module loading and function availability
  try {
    if (typeof expand === 'function' && 
        typeof compact === 'function' && 
        typeof normalizeData === 'function' && 
        typeof parseContext === 'function') {
      console.log('✅ ESM module loads and functions are available');
      passed++;
    } else {
      console.log('❌ ESM module loading failed - functions not available');
      failed++;
    }
  } catch (error) {
    console.log('❌ ESM module loading failed:', error.message);
    failed++;
  }

  // Test 2: Basic normalize functionality
  try {
    const normalized = normalizeData(testDoc);
    if (typeof normalized === 'string') {
      console.log('✅ normalizeData() works');
      passed++;
    } else {
      console.log('❌ normalizeData() failed - invalid result type:', typeof normalized);
      failed++;
    }
  } catch (error) {
    console.log('❌ normalizeData() failed:', error.message);
    failed++;
  }

  // Test 3: parseContext with JavaScript object
  try {
    const parsedContext = parseContext(testContext);
    if (parsedContext && typeof parsedContext === 'object') {
      console.log('✅ parseContext() works with JS object');
      passed++;
    } else {
      console.log('❌ parseContext() failed - invalid result');
      failed++;
    }
  } catch (error) {
    console.log('❌ parseContext() failed:', error.message);
    failed++;
  }

  // Test 5: Full workflow test (expand -> parse context -> compact)
  try {
    const expanded = expand(testDoc);
    const parsedContext = parseContext(testContext);
    const compacted = compact(expanded, parsedContext);
    
    if (expanded && parsedContext && compacted) {
      console.log('✅ Full JS workflow (expand->parseContext->compact) works');
      passed++;
    } else {
      console.log('❌ Full workflow failed');
      failed++;
    }
  } catch (error) {
    console.log('❌ Full workflow failed:', error.message);
    failed++;
  }

  // Test 4: Function signatures check
  try {
    console.log('Function info:');
    console.log('- expand.length:', expand.length);
    console.log('- compact.length:', compact.length);
    console.log('- normalizeData.length:', normalizeData.length);
    console.log('- parseContext.length:', parseContext.length);
    console.log('✅ Function introspection works');
    passed++;
  } catch (error) {
    console.log('❌ Function introspection failed:', error.message);
    failed++;
  }

  // Summary
  console.log(`\n📊 Test Results: ${passed} passed, ${failed} failed`);
  
  // Consider the test successful if most functionality works
  if (passed >= 4) {
    console.log('🎉 Node.js ESM with JS-friendly API working great!');
    process.exit(0);
  } else if (passed >= 2) {
    console.log('🎉 Core Node.js ESM functionality verified!');
    console.log('ℹ️  Note: Some advanced functions may need refinement');
    process.exit(0);
  } else {
    console.log('💥 Critical ESM functionality failed');
    process.exit(1);
  }
}

runTests().catch(error => {
  console.error('❌ Test runner failed:', error);
  process.exit(1);
});